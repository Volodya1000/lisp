from typing import List, Optional
from semantic.ast_nodes import *
from semantic.symbol_table import Environment, SymbolInfo

from .wasm_context import CompilerContext
from .wasm_stdlib import WasmStdLib
from .wasm_primitives import PrimitiveHandler


class CompilerError(Exception):
    pass


class StackFrame:
    """
    Инкапсулирует логику расчета смещений в стековом кадре (Env).
    Layout:
    [0..8]   - Ссылка на родительский Env
    [8..16]  - Параметр 1
    [16..24] - Параметр 2
    ...
    """
    WORD_SIZE = 8

    def __init__(self, params: List[str], local_vars_count: int):
        self.params = params
        # +1 слот для parent env
        self.total_slots = 1 + len(params) + local_vars_count
        self.size_bytes = self.total_slots * self.WORD_SIZE

    def get_param_offset(self, index: int) -> int:
        # param 0 лежит по смещению 8 (после parent env)
        return (index + 1) * self.WORD_SIZE


class WatBuilder:
    """Помощник для сборки WASM-кода без конкатенации строк"""

    def __init__(self):
        self.lines: List[str] = []

    def add(self, line: str):
        self.lines.append(line)

    def add_block(self, text: str):
        if text.strip():
            self.lines.append(text)

    def build(self) -> str:
        return "\n".join(self.lines)


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
            # Defun компилируются отдельно и добавляются в self.ctx.funcs_code
            if isinstance(node, DefunNode):
                node.accept(self)
            else:
                main_nodes.append(node)

        main_body = self._compile_block(main_nodes) if main_nodes else "f64.const 0.0"
        return self._build_final_module(nodes, main_body)

    def _build_final_module(self, nodes: List[ASTNode], main_body: str) -> str:
        # Использование WatBuilder для структуры модуля
        wb = WatBuilder()
        wb.add('(module')
        wb.add(WasmStdLib.get_imports())
        wb.add(WasmStdLib.get_memory_config())
        wb.add(self._get_type_definitions())
        wb.add(self._get_table_config())
        wb.add(WasmStdLib.get_globals(self.ctx.global_vars))
        wb.add(WasmStdLib.get_runtime_funcs())

        # Добавляем код всех функций (defun + lambda)
        for func_code in self.ctx.funcs_code:
            wb.add(func_code)

        wb.add(self._generate_main_func(nodes, main_body))
        wb.add(')')
        return wb.build()

    def _generate_main_func(self, nodes: List[ASTNode], main_body: str) -> str:
        wb = WatBuilder()
        wb.add('  (func $main (export "main") (result f64)')
        wb.add('    (local $env f64)')
        wb.add('    f64.const 0.0')
        wb.add('    local.set $env')

        # Инициализация глобальных функций (создание замыканий для Defun)
        for node in nodes:
            if isinstance(node, DefunNode):
                # Если функция есть в таблице (она там должна быть)
                try:
                    func_idx = self.ctx.table_entries.index(f"${node.name}")
                    wb.add(f"    ;; Init {node.name}")
                    wb.add(f"    f64.const {func_idx}.0")
                    wb.add(f"    f64.const 0.0")
                    wb.add(f"    call $std_cons")
                    wb.add(f"    global.set ${node.name}")
                except ValueError:
                    pass

        wb.add(self._indent(main_body, 4))
        wb.add('  )')
        return wb.build()

    def _scan_definitions(self, nodes: List[ASTNode]):
        for node in nodes:
            if isinstance(node, DefunNode):
                self.global_env.define(node.name, is_function=True)
                self.ctx.global_vars.add(node.name)
            elif isinstance(node, SetqNode):
                self.ctx.define_global(node.var_name)

    # --- Core Logic Extraction ---

    def _compile_function_base(self, name: str, params: List[str], body_nodes: List[ASTNode], env: Environment) -> str:
        """
        Единая логика компиляции тела функции (для Defun и Lambda).
        1. Входит в Env.
        2. Компилирует тело.
        3. Генерирует пролог (аллокация фрейма).
        4. Оборачивает в (func ...).
        """
        prev_env = self.ctx.current_env
        prev_is_inside = self.ctx.is_inside_func

        self.ctx.enter_function(env)

        body_wat = self._compile_block(body_nodes)

        # Используем StackFrame для расчета памяти
        # Добавляем запас +10 слотов для локальных переменных (let и т.д.)
        frame = StackFrame(params, env.current_var_index + 10)
        prologue = self._generate_prologue(frame)

        self.ctx.exit_function(prev_env, prev_is_inside)

        params_wat = "(param $env f64) " + " ".join(["(param f64)" for _ in params])

        wb = WatBuilder()
        wb.add(f'  (func {name} {params_wat} (result f64)')
        wb.add('    (local $new_env_addr i32)')
        wb.add(self._indent(prologue, 4))
        wb.add(self._indent(body_wat, 4))
        wb.add('  )')

        return wb.build()

    def _generate_prologue(self, frame: StackFrame) -> str:
        wb = WatBuilder()
        wb.add(f";; -- Alloc Frame (size {frame.size_bytes}) --")
        wb.add("global.get $heap_ptr")
        wb.add("local.tee $new_env_addr")
        wb.add(f"i32.const {frame.size_bytes}")
        wb.add("i32.add")
        wb.add("global.set $heap_ptr")

        wb.add(";; Store Parent Env at offset 0")
        wb.add("local.get $new_env_addr")
        wb.add("local.get $env")
        wb.add("f64.store")

        for i, param in enumerate(frame.params):
            offset = frame.get_param_offset(i)
            wb.add(f";; Store param '{param}' offset {offset}")
            wb.add("local.get $new_env_addr")
            wb.add(f"i32.const {offset}")
            wb.add("i32.add")
            wb.add(f"local.get {i + 1}")  # +1 потому что $env это param 0
            wb.add("f64.store")

        return wb.build()

    # --- Visitor Methods ---

    def visit_defun(self, node: DefunNode):
        func_name = f"${node.name}"
        self.ctx.register_function(func_name)

        func_env = Environment(parent=self.global_env)
        for param in node.params: func_env.define(param)

        # Вызов общей логики
        func_code = self._compile_function_base(func_name, node.params, node.body, func_env)
        self.ctx.funcs_code.append(func_code)
        return ""

    def visit_lambda(self, node: LambdaNode) -> str:
        name = self.ctx.get_lambda_name()
        func_idx = self.ctx.register_function(name)

        # Вызов общей логики (используем closure_env из ноды)
        func_code = self._compile_function_base(name, node.params, node.body, node.closure_env)
        self.ctx.funcs_code.append(func_code)

        # Формирование замыкания
        env_ptr = "local.get $new_env_addr\nf64.convert_i32_u" if self.ctx.is_inside_func else "local.get $env"
        return f"f64.const {func_idx}.0\n{env_ptr}\ncall $std_cons"

    def visit_symbol(self, node: SymbolNode) -> str:
        info = self.ctx.current_env.resolve(node.name)

        # 1. Локальная переменная или аргумент (найдено в Env)
        if info and (info.env_level > 0 or info.var_index is not None):
            addr_code = self._emit_var_addr(info)
            return f"{addr_code}\nf64.load"

        # 2. Глобальная переменная (известная)
        if node.name in self.ctx.global_vars or node.name in Environment.PRIMITIVES:
            return f"global.get ${node.name}"

        # 3. Ошибка (Strict Validation)
        raise CompilerError(f"Undefined variable: '{node.name}'")

    def visit_setq(self, node: SetqNode) -> str:
        info = self.ctx.current_env.resolve(node.var_name)
        val_code = node.value.accept(self)

        # Strict Logic: если переменной нет нигде, создаем глобальную (Lisp behavior),
        # но можно ужесточить. Пока оставим как создание глобальной.
        if not info or (info.env_level == 0 and info.var_index is None):
            if node.var_name not in self.ctx.global_vars:
                self.ctx.define_global(node.var_name)
            return f"{val_code}\nglobal.set ${node.var_name}\nglobal.get ${node.var_name}"

        code = f"{val_code}\n"
        code += "global.set $scratch\n"
        code += self._emit_var_addr(info) + "\n"
        code += "global.get $scratch\n"
        code += "f64.store\n"
        code += "global.get $scratch"
        return code

    # --- Helpers & Standard Visitors ---

    def _compile_block(self, nodes: List[ASTNode]) -> str:
        if not nodes: return "f64.const 0.0"
        wb = WatBuilder()
        for i, node in enumerate(nodes):
            wb.add(node.accept(self))
            if i < len(nodes) - 1: wb.add("drop")
        return wb.build()

    def _emit_var_addr(self, info: SymbolInfo) -> str:
        base_ptr = "local.get $new_env_addr" if self.ctx.is_inside_func else "local.get $env\ni32.trunc_f64_u"
        code = [base_ptr]
        hops = self.ctx.current_env.level - info.env_level
        for _ in range(hops):
            code.append("f64.load")
            code.append("i32.trunc_f64_u")

        # Используем логику StackFrame и здесь, но пока просто offset
        offset = (info.var_index + 1) * 8
        code.append(f"i32.const {offset}")
        code.append("i32.add")
        return "\n".join(code)

    def _indent(self, code: str, spaces: int) -> str:
        prefix = " " * spaces
        return "\n".join([prefix + line for line in code.split('\n')])

    def _get_type_definitions(self) -> str:
        types = ""
        for i in range(11):
            params = " ".join(["(param f64)"] * (i + 1))
            types += f"  (type $type_{i} (func {params} (result f64)))\n"
        return types

    def _get_table_config(self) -> str:
        count = len(self.ctx.table_entries)
        if count == 0: return "  (table 0 funcref)\n"
        funcs_list = " ".join(self.ctx.table_entries)
        return f"  (table {count} funcref)\n  (elem (i32.const 0) {funcs_list})\n"

    # --- Остальные визиторы без изменений логики, но используют accept ---

    def visit_call(self, node: CallNode) -> str:
        if isinstance(node.func, SymbolNode) and node.func.name in Environment.PRIMITIVES:
            return self.visit_prim_call(PrimCallNode(node.func.name, node.args))

        func_calc = node.func.accept(self)
        args_calc = [arg.accept(self) for arg in node.args]
        arity = len(node.args)

        wb = WatBuilder()
        wb.add(f";; -- Call Indirect (arity {arity}) --")
        wb.add(func_calc)
        wb.add("global.set $scratch")
        wb.add("global.get $scratch")
        wb.add("call $std_cdr")
        for arg in args_calc: wb.add(arg)
        wb.add("global.get $scratch")
        wb.add("call $std_car")
        wb.add("i32.trunc_f64_u")
        wb.add(f"call_indirect (type $type_{arity})")
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
        wb.add("global.get $heap_ptr")
        wb.add(f"global.get $heap_ptr\ni32.const {length}\ni32.store")
        for i, byte in enumerate(text_bytes):
            wb.add(f"global.get $heap_ptr\ni32.const {4 + i}\ni32.add\ni32.const {byte}\ni32.store8")
        wb.add(f"global.get $heap_ptr\ni32.const {4 + length}\ni32.add\nglobal.set $heap_ptr")
        wb.add("f64.convert_i32_u")
        return wb.build()

    def visit_progn(self, node: PrognNode):
        return self._compile_block(node.body)

    def visit_cond(self, node: CondNode):
        if not node.clauses: return "f64.const 0.0"
        return self._compile_cond_recursive(node.clauses)

    def _compile_cond_recursive(self, clauses: List[tuple]) -> str:
        if not clauses: return "f64.const 0.0"
        pred, body = clauses[0]
        check = f"{pred.accept(self)}\nf64.const 0.0\nf64.ne"
        then_code = self._compile_block(body)
        else_code = self._compile_cond_recursive(clauses[1:])
        return f"{check}\n(if (result f64) (then {self._indent(then_code, 2)}) (else {self._indent(else_code, 2)}))"

    def visit_logic(self, node: LogicNode):
        if node.op == 'and': return self._compile_and(node.args)
        return self._compile_or(node.args)

    def _compile_and(self, args):
        if not args: return "f64.const 1.0"
        curr, rest = args[0], args[1:]
        if not rest: return curr.accept(self)
        return f"{curr.accept(self)}\nf64.const 0.0\nf64.ne\n(if (result f64) (then {self._indent(self._compile_and(rest), 2)}) (else f64.const 0.0))"

    def _compile_or(self, args):
        if not args: return "f64.const 0.0"
        curr, rest = args[0], args[1:]
        if not rest: return curr.accept(self)
        return f"{curr.accept(self)}\nglobal.set $scratch\nglobal.get $scratch\nf64.const 0.0\nf64.ne\n(if (result f64) (then global.get $scratch) (else {self._indent(self._compile_or(rest), 2)}))"