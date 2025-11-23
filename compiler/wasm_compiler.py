from typing import List, Optional
from contextlib import contextmanager
from semantic.ast_nodes import *
from semantic.symbol_table import Environment, SymbolInfo

from .wasm_context import CompilerContext
from .wasm_stdlib import WasmStdLib
from .wasm_primitives import PrimitiveHandler
from .wasm_types import WasmType, OpCode


class CompilerError(Exception):
    pass


class FramePolicy:
    """
    Единый источник правды о структуре стекового кадра в памяти (Environment).
    Layout: [ParentEnvPtr (8b)] [Var0 (8b)] [Var1 (8b)] ...
    """
    WORD_SIZE = 8
    ENV_PTR_OFFSET = 0

    @staticmethod
    def get_offset(var_index: int) -> int:
        # Индекс 0 идет сразу после EnvPtr (смещение 0)
        return FramePolicy.WORD_SIZE + (var_index * FramePolicy.WORD_SIZE)

    @staticmethod
    def calculate_size(vars_count: int) -> int:
        # 1 слот под ParentEnvPtr + переменные
        return (1 + vars_count) * FramePolicy.WORD_SIZE


class WatBuilder:
    """Структурированный билдер для генерации WAT с поддержкой блоков"""

    def __init__(self):
        self.lines: List[str] = []

    def raw(self, line: str):
        """Для вставки готовых кусков кода (например, от вложенных visitor)"""
        if line:
            self.lines.append(line)

    def emit(self, op: str, type_hint: Optional[str] = None):
        """Пример: emit(OpCode.ADD, WasmType.F64) -> f64.add"""
        if type_hint:
            self.lines.append(f"{type_hint}.{op}")
        else:
            self.lines.append(op)

    def emit_const(self, value, type_hint: str = WasmType.F64):
        self.lines.append(f"{type_hint}.const {value}")

    def emit_get(self, name: str, scope: str = 'local'):
        self.lines.append(f"{scope}.get {name}")

    def emit_set(self, name: str, scope: str = 'local'):
        self.lines.append(f"{scope}.set {name}")

    def emit_call(self, func_name: str):
        self.lines.append(f"call {func_name}")

    def build(self) -> str:
        return "\n".join(self.lines)

    # --- Control Flow Managers ---

    @contextmanager
    def if_block(self, result_type: str = 'f64'):
        """
        Генерирует структуру: (if (result f64) (then ... )
        Использовать вместе с else_block().
        """
        self.raw(f"(if (result {result_type})")
        self.raw("(then")
        yield
        self.raw(")")  # Закрываем then

    @contextmanager
    def else_block(self):
        """
        Генерирует структуру: (else ... ) )
        Должен идти сразу после if_block. Закрывает весь if.
        """
        self.raw("(else")
        yield
        self.raw(")")  # Закрываем else
        self.raw(")")  # Закрываем весь if


class WasmCompiler(ASTVisitor):
    def __init__(self):
        self.global_env = Environment()
        self._init_primitives()
        self.ctx = CompilerContext(self.global_env)
        self.prim_handler = PrimitiveHandler()

    def _init_primitives(self):
        for name in Environment.PRIMITIVES:
            self.global_env.define(name, is_function=True)

    def compile(self, nodes: List[ASTNode]) -> str:
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

        # Генерация определений типов (собранных в процессе компиляции Defun/Call)
        wb.raw(self.ctx.type_registry.generate_definitions())

        wb.raw(self._get_table_config())
        wb.raw(WasmStdLib.get_globals(self.ctx.global_vars))
        wb.raw(WasmStdLib.get_runtime_funcs())

        for func_code in self.ctx.funcs_code:
            wb.raw(func_code)

        wb.raw(self._generate_main_func(nodes, main_body))
        wb.raw(')')
        return wb.build()

    def _generate_main_func(self, nodes: List[ASTNode], main_body: str) -> str:
        wb = WatBuilder()
        wb.raw('  (func $main (export "main") (result f64)')
        wb.raw('    (local $env f64)')
        wb.emit_const('0.0')
        wb.emit_set('$env')

        # Инициализация глобальных функций (замыкания)
        for node in nodes:
            if isinstance(node, DefunNode):
                try:
                    func_idx = self.ctx.table_entries.index(f"${node.name}")
                    wb.raw(f"    ;; Init {node.name}")
                    wb.emit_const(f"{func_idx}.0")
                    wb.emit_const('0.0')
                    wb.emit_call('$std_cons')
                    wb.emit_set(f"${node.name}", 'global')
                except ValueError:
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

    # --- Core Logic ---

    def _compile_function_base(self, name: str, params: List[str], body_nodes: List[ASTNode], env: Environment) -> str:
        prev_env = self.ctx.current_env
        prev_is_inside = self.ctx.is_inside_func

        self.ctx.enter_function(env)

        body_wat = self._compile_block(body_nodes)

        # Регистрируем тип (arity)
        arity = len(params)
        self.ctx.type_registry.get_or_register(arity)

        # Расчет фрейма (с запасом +10 для локальных let/loop переменных)
        frame_size = FramePolicy.calculate_size(env.current_var_index + 10)
        prologue = self._generate_prologue(params, frame_size)

        self.ctx.exit_function(prev_env, prev_is_inside)

        params_wat = "(param $env f64) " + " ".join([f"(param {WasmType.F64})" for _ in params])

        wb = WatBuilder()
        wb.raw(f'  (func {name} {params_wat} (result f64)')
        wb.raw('    (local $new_env_addr i32)')
        wb.raw(self._indent(prologue, 4))
        wb.raw(self._indent(body_wat, 4))
        wb.raw('  )')

        return wb.build()

    def _generate_prologue(self, params: List[str], size_bytes: int) -> str:
        wb = WatBuilder()
        wb.raw(f";; -- Alloc Frame (size {size_bytes}) --")

        wb.emit_get('$heap_ptr', 'global')
        wb.emit(OpCode.TEE, 'local')
        wb.raw('$new_env_addr')

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
            wb.raw(f"local.get {i + 1}")  # +1 так как $env это параметр 0
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

    # --- Visitor Methods ---

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
        func_code = self._compile_function_base(name, node.params, node.body, node.closure_env)
        self.ctx.funcs_code.append(func_code)

        env_ptr = "local.get $new_env_addr\nf64.convert_i32_u" if self.ctx.is_inside_func else "local.get $env"
        return f"f64.const {func_idx}.0\n{env_ptr}\ncall $std_cons"

    def visit_symbol(self, node: SymbolNode) -> str:
        info = self.ctx.current_env.resolve(node.name)
        wb = WatBuilder()

        if info and (info.env_level > 0 or info.var_index is not None):
            addr_code = self._emit_var_addr(info)
            wb.raw(addr_code)
            wb.emit(OpCode.LOAD, WasmType.F64)
            return wb.build()

        if node.name in self.ctx.global_vars or node.name in Environment.PRIMITIVES:
            wb.emit_get(f"${node.name}", 'global')
            return wb.build()

        raise CompilerError(f"Undefined variable: '{node.name}'")

    def visit_setq(self, node: SetqNode) -> str:
        info = self.ctx.current_env.resolve(node.var_name)
        val_code = node.value.accept(self)
        wb = WatBuilder()

        if not info or (info.env_level == 0 and info.var_index is None):
            if node.var_name not in self.ctx.global_vars:
                self.ctx.define_global(node.var_name)
            wb.raw(val_code)
            wb.emit_set(f"${node.var_name}", 'global')
            wb.emit_get(f"${node.var_name}", 'global')
            return wb.build()

        wb.raw(val_code)
        wb.emit_set('$scratch', 'global')
        wb.raw(self._emit_var_addr(info))
        wb.emit_get('$scratch', 'global')
        wb.emit(OpCode.STORE, WasmType.F64)
        wb.emit_get('$scratch', 'global')
        return wb.build()

    # --- Control Flow Visitors (Refactored to use Builder) ---

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

        # 1. Check Condition
        wb.raw(pred.accept(self))
        wb.emit_const('0.0')
        # ИСПРАВЛЕНИЕ: Добавлен WasmType.F64
        wb.emit(OpCode.NE, WasmType.F64)

        # 2. Structure If/Else
        with wb.if_block():
            # Тело ветки then
            wb.raw(self._compile_block(body))

        with wb.else_block():
            # Рекурсия для else if
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

        # (and curr rest...) => if curr != 0 then (and rest...) else 0
        wb.raw(curr.accept(self))
        wb.emit_const('0.0')
        # ИСПРАВЛЕНИЕ: Добавлен WasmType.F64
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

        # (or curr rest...) => temp=curr; if temp != 0 then temp else (or rest...)
        wb.raw(curr.accept(self))
        wb.emit_set('$scratch', 'global')

        wb.emit_get('$scratch', 'global')
        wb.emit_const('0.0')
        # ИСПРАВЛЕНИЕ: Добавлен WasmType.F64
        wb.emit(OpCode.NE, WasmType.F64)

        with wb.if_block():
            wb.emit_get('$scratch', 'global')
        with wb.else_block():
            self._compile_or(rest, wb)

    # --- Other Visitors ---

    def visit_call(self, node: CallNode) -> str:
        if isinstance(node.func, SymbolNode) and node.func.name in Environment.PRIMITIVES:
            return self.visit_prim_call(PrimCallNode(node.func.name, node.args))

        func_calc = node.func.accept(self)
        arity = len(node.args)

        type_name = self.ctx.type_registry.get_or_register(arity)

        wb = WatBuilder()
        wb.raw(f";; -- Call Indirect (arity {arity}) --")
        wb.raw(func_calc)
        wb.emit_set('$scratch', 'global')
        wb.emit_get('$scratch', 'global')
        wb.emit_call('$std_cdr')

        for arg in node.args:
            wb.raw(arg.accept(self))

        wb.emit_get('$scratch', 'global')
        wb.emit_call('$std_car')
        wb.emit(OpCode.TRUNC_U, WasmType.I32)
        wb.raw(f"call_indirect (type {type_name})")
        return wb.build()

    def visit_prim_call(self, node: PrimCallNode) -> str:
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
        text_bytes = node.value.encode('utf-8')
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