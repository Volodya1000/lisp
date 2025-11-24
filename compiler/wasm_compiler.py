import logging
import sys
from typing import List

# --- LOGGING SETUP ---
# Настраиваем логгер, чтобы он выводил информацию в stderr
logging.basicConfig(
    level=logging.DEBUG,
    format='[%(levelname)s] %(message)s',
    stream=sys.stderr
)
logger = logging.getLogger("WasmCompiler")
# ---------------------

from semantic.ast_nodes import *
from semantic.symbol_table import Environment, SymbolInfo
from .frame_policy import FramePolicy

from .wasm_context import CompilerContext
from .wasm_stdlib import WasmStdLib
from .wasm_primitives import PrimitiveHandler
from .wasm_types import WasmType, OpCode
from .wat_builder import WatBuilder


class CompilerError(Exception):
    pass


class WasmCompiler(ASTVisitor):
    def __init__(self):
        self.global_env = Environment()
        self._init_primitives()
        self.ctx = CompilerContext(self.global_env)
        self.prim_handler = PrimitiveHandler()
        logger.info("Compiler initialized.")

    def _init_primitives(self):
        for name in Environment.PRIMITIVES:
            self.global_env.define(name, is_function=True)

    def compile(self, nodes: List[ASTNode]) -> str:
        logger.info("Starting compilation...")
        # СБРОС контекста перед компиляцией
        self.ctx.type_registry.registered_types.clear()

        self._scan_definitions(nodes)

        main_nodes: List[ASTNode] = []
        for node in nodes:
            if isinstance(node, DefunNode):
                node.accept(self)
            else:
                main_nodes.append(node)

        main_body = self._compile_block(main_nodes) if main_nodes else "f64.const 0.0"
        return self._build_final_module(nodes, main_body)

    def _build_final_module(self, nodes: List[ASTNode], main_body: str) -> str:
        wb = WatBuilder()
        wb.raw('(module')
        wb.raw(WasmStdLib.get_imports())
        wb.raw(WasmStdLib.get_memory_config())

        # --- DEBUG LOGGING: TYPE REGISTRY ---
        logger.info("=== Generating Type Definitions ===")
        type_defs = self.ctx.type_registry.generate_definitions()
        # Здесь можно вывести сами типы, если у registry есть метод для отображения
        wb.raw(type_defs)

        # --- DEBUG LOGGING: TABLE ENTRIES ---
        logger.info("=== Function Table Layout ===")
        for idx, name in enumerate(self.ctx.table_entries):
            logger.info(f"  Table Index {idx}: {name}")

        wb.raw(self._get_table_config())
        wb.raw(WasmStdLib.get_globals(self.ctx.global_vars))
        wb.raw(WasmStdLib.get_runtime_funcs())

        for func_code in self.ctx.funcs_code:
            wb.raw(func_code)

        wb.raw(self._generate_main_func(nodes, main_body))
        wb.raw(')')

        result = wb.build()
        logger.info("Compilation finished successfully.")
        return result

    def _generate_main_func(self, nodes: List[ASTNode], main_body: str) -> str:
        wb = WatBuilder()
        wb.raw('  (func $entry (export "main") (result f64)')
        wb.raw('    (local $env f64)')
        wb.raw(self._get_closure_locals_defs())

        wb.emit_const('0.0')
        wb.emit_set('$env')

        for node in nodes:
            if isinstance(node, DefunNode):
                try:
                    func_idx = self.ctx.table_entries.index(f"${node.name}")
                    wb.raw(f"    ;; Init {node.name} (Table Idx: {func_idx})")
                    wb.emit_const(f"{func_idx}.0")
                    wb.emit_const('0.0')
                    wb.emit_call('$std_cons')
                    wb.emit_set(f"${node.name}", 'global')
                except ValueError:
                    logger.warning(f"Function ${node.name} not found in table entries during main init.")
                    pass

        wb.raw(self._indent(main_body, 4))
        wb.raw('  )')
        return wb.build()

    def _scan_definitions(self, nodes: List[ASTNode]):
        for node in nodes:
            if isinstance(node, DefunNode):
                self.global_env.define(node.name, is_function=True)
                self.ctx.global_vars.add(node.name)
            elif isinstance(node, SetqNode):
                self.ctx.define_global(node.var_name)

    def _compile_function_base(self, name: str, params: List[str], body_nodes: List[ASTNode], env: Environment) -> str:
        logger.debug(f"Compiling function '{name}' with params: {params}")

        prev_env = self.ctx.current_env
        prev_is_inside = self.ctx.is_inside_func

        self.ctx.enter_function(env)

        body_wat = self._compile_block(body_nodes)

        # Регистрируем тип функции
        arity = len(params)
        type_name = self.ctx.type_registry.get_or_register(arity)

        logger.info(f"FUNC DEF: '{name}' | Arity: {arity} | Assigned Type: {type_name}")

        frame_size = FramePolicy.calculate_size(env.current_var_index + 10)
        prologue = self._generate_prologue(params, frame_size)

        self.ctx.exit_function(prev_env, prev_is_inside)

        params_wat = "(param $env f64) " + " ".join([f"(param {WasmType.F64})" for _ in params])

        wb = WatBuilder()
        wb.raw(f'  (func {name} {params_wat} (result f64)')
        wb.raw('    (local $new_env_addr i32)')
        wb.raw(self._get_closure_locals_defs())

        wb.raw(self._indent(prologue, 4))
        wb.raw(self._indent(body_wat, 4))
        wb.raw('  )')

        return wb.build()

    def _generate_prologue(self, params: List[str], size_bytes: int) -> str:
        wb = WatBuilder()
        wb.raw(f";; -- Alloc Frame (size {size_bytes}) --")

        wb.emit_get('$heap_ptr', 'global')
        wb.raw("local.tee $new_env_addr")

        wb.emit_const(size_bytes, WasmType.I32)
        wb.emit(OpCode.ADD, WasmType.I32)
        wb.emit_set('$heap_ptr', 'global')

        wb.raw(";; Store Parent Env")
        wb.emit_get('$new_env_addr')
        wb.emit_get('$env')
        wb.emit(OpCode.STORE, WasmType.F64)

        for i, param in enumerate(params):
            offset = FramePolicy.get_offset(i)
            wb.raw(f";; Store param '{param}' offset {offset}")
            wb.emit_get('$new_env_addr')
            wb.emit_const(offset, WasmType.I32)
            wb.emit(OpCode.ADD, WasmType.I32)
            wb.raw(f"local.get {i + 1}")
            wb.emit(OpCode.STORE, WasmType.F64)

        return wb.build()

    def _emit_var_addr(self, info: SymbolInfo) -> str:
        wb = WatBuilder()
        base_ptr = "local.get $new_env_addr" if self.ctx.is_inside_func else "local.get $env\ni32.trunc_f64_u"
        wb.raw(base_ptr)

        hops = self.ctx.current_env.level - info.env_level
        for _ in range(hops):
            wb.emit(OpCode.LOAD, WasmType.F64)
            wb.emit(OpCode.TRUNC_U, WasmType.I32)

        offset = FramePolicy.get_offset(info.var_index)
        wb.emit_const(offset, WasmType.I32)
        wb.emit(OpCode.ADD, WasmType.I32)

        return wb.build()

    def visit_defun(self, node: DefunNode):
        func_name = f"${node.name}"
        self.ctx.register_function(func_name)
        func_env = Environment(parent=self.global_env)
        for param in node.params: func_env.define(param)

        func_code = self._compile_function_base(func_name, node.params, node.body, func_env)
        self.ctx.funcs_code.append(func_code)
        return ""

    def visit_lambda(self, node: LambdaNode) -> str:
        name = self.ctx.get_lambda_name()
        func_idx = self.ctx.register_function(name)

        logger.debug(f"Visiting LAMBDA: generated name '{name}', table index {func_idx}")

        func_code = self._compile_function_base(name, node.params, node.body, node.closure_env)
        self.ctx.funcs_code.append(func_code)

        env_ptr = "local.get $new_env_addr\nf64.convert_i32_u" if self.ctx.is_inside_func else "local.get $env"
        return f"f64.const {func_idx}.0\n{env_ptr}\ncall $std_cons"

    def visit_symbol(self, node: SymbolNode) -> str:
        info = self.ctx.current_env.resolve(node.name)
        wb = WatBuilder()

        if info and info.env_level > 0:
            addr_code = self._emit_var_addr(info)
            wb.raw(addr_code)
            wb.emit(OpCode.LOAD, WasmType.F64)
            return wb.build()

        if node.name in self.ctx.global_vars:
            wb.emit_get(f"${node.name}", 'global')
            return wb.build()

        if node.name in Environment.PRIMITIVES:
            raise CompilerError(f"Cannot use primitive '{node.name}' as a value.")

        raise CompilerError(f"Undefined variable: '{node.name}'")

    def visit_setq(self, node: SetqNode) -> str:
        info = self.ctx.current_env.resolve(node.var_name)
        val_code = node.value.accept(self)
        wb = WatBuilder()

        if info and info.env_level > 0:
            wb.raw(val_code)
            wb.emit_set('$scratch', 'global')
            wb.raw(self._emit_var_addr(info))
            wb.emit_get('$scratch', 'global')
            wb.emit(OpCode.STORE, WasmType.F64)
            wb.emit_get('$scratch', 'global')
            return wb.build()

        if node.var_name not in self.ctx.global_vars:
            self.ctx.define_global(node.var_name)

        wb.raw(val_code)
        wb.emit_set(f"${node.var_name}", 'global')
        wb.emit_get(f"${node.var_name}", 'global')
        return wb.build()

    # --- Control Flow ---

    def visit_if(self, node: CondNode) -> str:
        wb = WatBuilder()
        wb.raw(node.condition.accept(self))
        wb.emit_const('0.0')
        wb.emit(OpCode.NE, WasmType.F64)
        with wb.if_block():
            wb.raw(node.then_branch.accept(self))
        with wb.else_block():
            if node.else_branch:
                wb.raw(node.else_branch.accept(self))
            else:
                wb.emit_const('0.0')
        return wb.build()

    def visit_cond(self, node: CondNode) -> str:
        wb = WatBuilder()
        if not node.clauses:
            wb.emit_const('0.0')
            return wb.build()
        self._compile_cond_recursive(node.clauses, wb)
        return wb.build()

    def _compile_cond_recursive(self, clauses: List[tuple], wb: WatBuilder):
        if not clauses:
            wb.emit_const('0.0')
            return
        pred, body = clauses[0]
        wb.raw(pred.accept(self))
        wb.emit_const('0.0')
        wb.emit(OpCode.NE, WasmType.F64)
        with wb.if_block():
            wb.raw(self._compile_block(body))
        with wb.else_block():
            self._compile_cond_recursive(clauses[1:], wb)

    def visit_logic(self, node: LogicNode) -> str:
        wb = WatBuilder()
        if node.op == 'and':
            self._compile_and(node.args, wb)
        else:
            self._compile_or(node.args, wb)
        return wb.build()

    def _compile_and(self, args: List[ASTNode], wb: WatBuilder):
        if not args:
            wb.emit_const('1.0')
            return
        curr, rest = args[0], args[1:]
        if not rest:
            wb.raw(curr.accept(self))
            return
        wb.raw(curr.accept(self))
        wb.emit_const('0.0')
        wb.emit(OpCode.NE, WasmType.F64)
        with wb.if_block():
            self._compile_and(rest, wb)
        with wb.else_block():
            wb.emit_const('0.0')

    def _compile_or(self, args: List[ASTNode], wb: WatBuilder):
        if not args:
            wb.emit_const('0.0')
            return
        curr, rest = args[0], args[1:]
        if not rest:
            wb.raw(curr.accept(self))
            return
        wb.raw(curr.accept(self))
        wb.emit_set('$scratch', 'global')
        wb.emit_get('$scratch', 'global')
        wb.emit_const('0.0')
        wb.emit(OpCode.NE, WasmType.F64)
        with wb.if_block():
            wb.emit_get('$scratch', 'global')
        with wb.else_block():
            self._compile_or(rest, wb)

    # --- Calls ---

    def visit_call(self, node: CallNode) -> str:
        # 1. Обработка примитивов
        if isinstance(node.func, SymbolNode) and node.func.name in Environment.PRIMITIVES:
            return self.visit_prim_call(PrimCallNode(node.func.name, node.args))

        # 2. Обработка IF
        if isinstance(node.func, SymbolNode) and node.func.name == 'if':
            wb = WatBuilder()
            if len(node.args) < 2: raise CompilerError("if requires at least 2 args")
            wb.raw(node.args[0].accept(self))
            wb.emit_const('0.0')
            wb.emit(OpCode.NE, WasmType.F64)
            with wb.if_block():
                wb.raw(node.args[1].accept(self))
            with wb.else_block():
                if len(node.args) > 2:
                    wb.raw(node.args[2].accept(self))
                else:
                    wb.emit_const('0.0')
            return wb.build()

        # --- General Indirect Call ---

        current_depth = self.ctx.call_depth
        if current_depth >= 20: raise CompilerError("Max call nesting depth exceeded (20)")
        closure_local = f"$closure_{current_depth}"
        self.ctx.call_depth += 1

        try:
            wb = WatBuilder()

            # DEBUG LOGGING FOR INDIRECT CALLS
            call_target_name = "Unknown Expression"
            if isinstance(node.func, SymbolNode):
                call_target_name = node.func.name

            # 3. FUNCALL (явный вызов)
            if isinstance(node.func, SymbolNode) and node.func.name == 'funcall':
                if not node.args: raise CompilerError("funcall requires args")
                func_expr_code = node.args[0].accept(self)
                call_args = node.args[1:]
                call_type = "Explicit FUNCALL"
            # 4. Implicit Call
            else:
                func_expr_code = node.func.accept(self)
                call_args = node.args
                call_type = "Implicit Call"

            arity = len(call_args)
            # Здесь мы регистрируем тип вызова. Если функция в рантайме ожидает другое кол-во аргументов,
            # WASM выбросит indirect call type mismatch.
            type_name = self.ctx.type_registry.get_or_register(arity)

            logger.info(
                f"CALL SITE: {call_type} to '{call_target_name}' | Args (Arity): {arity} | Expects Type: {type_name}")

            wb.raw(f";; -- Call/Funcall (arity {arity}) --")

            # 1. Вычисляем и сохраняем ЗАМЫКАНИЕ
            wb.raw(func_expr_code)
            wb.emit_set(closure_local)

            # 2. Кладем Env (Param 0) на стек
            wb.emit_get(closure_local)
            wb.emit_call('$std_cdr')

            # 3. Кладем Аргументы (Param 1..N) на стек
            for i, arg in enumerate(call_args):
                # logger.debug(f"  Compiling arg {i} for call to {call_target_name}")
                wb.raw(arg.accept(self))

            # 4. Получаем Индекс функции из замыкания
            wb.emit_get(closure_local)
            wb.emit_call('$std_car')
            wb.emit(OpCode.TRUNC_U, WasmType.I32)

            # 5. Вызываем
            wb.raw(f"call_indirect (type {type_name})")
            return wb.build()

        finally:
            self.ctx.call_depth -= 1

    def visit_prim_call(self, node: PrimCallNode) -> str:
        # logger.debug(f"Primitive call: {node.prim_name} with {len(node.args)} args")
        return self.prim_handler.handle(node.prim_name, node.args, self)

    def visit_number(self, node: NumberNode):
        return f"f64.const {node.value}"

    def visit_nil(self, node: NilNode):
        return "f64.const 0.0"

    def visit_true(self, node: TrueNode):
        return "f64.const 1.0"

    def visit_quote(self, node: QuoteNode):
        if isinstance(node.expr, NumberNode): return f"f64.const {node.expr.value}"
        if isinstance(node.expr, ListNode): return self.visit_list(node.expr)
        if isinstance(node.expr, StringNode): return self.visit_string(node.expr)
        return "f64.const 0.0"

    def visit_list(self, node: ListNode):
        code = "f64.const 0.0"
        for elem in reversed(node.elements):
            elem_code = self._compile_quoted_element(elem)
            code = f"{elem_code}\n{code}\ncall $std_cons"
        return code

    def _compile_quoted_element(self, node: ASTNode):
        if isinstance(node, NumberNode): return f"f64.const {node.value}"
        if isinstance(node, ListNode): return self.visit_list(node)
        if isinstance(node, StringNode): return self.visit_string(node)
        return "f64.const 0.0"

    def visit_string(self, node: StringNode) -> str:
        processed_value = node.value.replace('\\n', '\n').replace('\\t', '\t').replace('\\"', '"')

        text_bytes = processed_value.encode('utf-8')
        length = len(text_bytes)
        wb = WatBuilder()
        wb.emit_get('$heap_ptr', 'global')
        wb.emit_get('$heap_ptr', 'global')
        wb.emit_const(length, WasmType.I32)
        wb.emit(OpCode.STORE, WasmType.I32)
        for i, byte in enumerate(text_bytes):
            wb.emit_get('$heap_ptr', 'global')
            wb.emit_const(4 + i, WasmType.I32)
            wb.emit(OpCode.ADD, WasmType.I32)
            wb.emit_const(byte, WasmType.I32)
            wb.raw("i32.store8")
        wb.emit_get('$heap_ptr', 'global')
        wb.emit_const(4 + length, WasmType.I32)
        wb.emit(OpCode.ADD, WasmType.I32)
        wb.emit_set('$heap_ptr', 'global')
        wb.emit(OpCode.CONVERT_U, WasmType.F64)
        return wb.build()

    def visit_progn(self, node: PrognNode):
        return self._compile_block(node.body)

    # --- Helpers ---

    def _compile_block(self, nodes: List[ASTNode]) -> str:
        if not nodes: return "f64.const 0.0"
        wb = WatBuilder()
        for i, node in enumerate(nodes):
            wb.raw(node.accept(self))
            if i < len(nodes) - 1: wb.emit(OpCode.DROP)
        return wb.build()

    def _get_table_config(self) -> str:
        count = len(self.ctx.table_entries)
        if count == 0: return "  (table 0 funcref)\n"
        funcs_list = " ".join(self.ctx.table_entries)
        return f"  (table {count} funcref)\n  (elem (i32.const 0) {funcs_list})\n"

    def _indent(self, code: str, spaces: int) -> str:
        prefix = " " * spaces
        return "\n".join([prefix + line for line in code.split('\n')])

    def _get_closure_locals_defs(self) -> str:
        return "\n".join([f"    (local $closure_{i} f64)" for i in range(20)])