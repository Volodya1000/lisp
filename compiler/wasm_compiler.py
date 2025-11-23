from semantic.ast_nodes import *
from semantic.symbol_table import Environment, SymbolInfo

from compiler.wasm_context import CompilerContext
from compiler.wasm_stdlib import WasmStdLib
from compiler.wasm_primitives import PrimitiveHandler


class WasmCompiler(ASTVisitor):
    def __init__(self):
        self.global_env = Environment()
        self._init_primitives()

        # Инициализация контекста и обработчиков
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

        return self._generate_module_structure(nodes, main_body)

    def _generate_module_structure(self, nodes: List[ASTNode], main_body: str) -> str:
        # Сборка итогового WAT файла
        wat = '(module\n'
        wat += WasmStdLib.get_imports()
        wat += WasmStdLib.get_memory_config()
        wat += self._get_type_definitions()
        wat += self._get_table_config()
        wat += WasmStdLib.get_globals(self.ctx.global_vars)
        wat += WasmStdLib.get_runtime_funcs()

        for func_code in self.ctx.funcs_code:
            wat += func_code + '\n'

        wat += self._generate_main_func(nodes, main_body)
        wat += ')'
        return wat

    def _generate_main_func(self, nodes: List[ASTNode], main_body: str) -> str:
        code = '  (func $main (export "main") (result f64)\n'
        code += '    (local $env f64)\n'
        code += '    f64.const 0.0\n'
        code += '    local.set $env\n'

        # Инициализация глобальных функций (помещение их в замыкания)
        for node in nodes:
            if isinstance(node, DefunNode):
                try:
                    func_idx = self.ctx.table_entries.index(f"${node.name}")
                    code += f"    ;; Init {node.name}\n"
                    code += f"    f64.const {func_idx}.0\n"
                    code += f"    f64.const 0.0\n"
                    code += f"    call $std_cons\n"
                    code += f"    global.set ${node.name}\n"
                except ValueError:
                    pass  # Функция могла быть не зарегистрирована в таблице, если она не косвенная, но мы регистрируем все Defun

        code += self._indent(main_body, 4)
        code += '\n  )\n'
        return code

    def _scan_definitions(self, nodes: List[ASTNode]):
        for node in nodes:
            if isinstance(node, DefunNode):
                self.global_env.define(node.name, is_function=True)
                self.ctx.global_vars.add(node.name)
            elif isinstance(node, SetqNode):
                self.ctx.define_global(node.var_name)

    # --- Helpers ---

    def _get_type_definitions(self) -> str:
        types = ""
        for i in range(11):
            params = " ".join(["(param f64)"] * (i + 1))
            types += f"  (type $type_{i} (func {params} (result f64)))\n"
        return types

    def _get_table_config(self) -> str:
        count = len(self.ctx.table_entries)
        if count == 0:
            return "  (table 0 funcref)\n"
        funcs_list = " ".join(self.ctx.table_entries)
        return f"  (table {count} funcref)\n  (elem (i32.const 0) {funcs_list})\n"

    def _compile_block(self, nodes: List[ASTNode]) -> str:
        if not nodes: return "f64.const 0.0"
        lines = []
        for i, node in enumerate(nodes):
            res = node.accept(self)
            lines.append(res)
            if i < len(nodes) - 1: lines.append("drop")
        return "\n".join(lines)

    def _indent(self, code: str, spaces: int) -> str:
        prefix = " " * spaces
        return "\n".join([prefix + line for line in code.split('\n')])

    def _alloc_frame(self, params: List[str], vars_count: int) -> str:
        # Логика выделения фрейма стека в куче
        total_slots = 1 + vars_count
        size_bytes = total_slots * 8

        code = [
            f";; -- Alloc Frame (size {size_bytes}) --",
            "global.get $heap_ptr",
            "local.tee $new_env_addr",
            f"i32.const {size_bytes}",
            "i32.add",
            "global.set $heap_ptr",
            ";; Store Parent Env",
            "local.get $new_env_addr",
            "local.get $env",
            "f64.store"
        ]

        for i, param in enumerate(params):
            offset = (i + 1) * 8
            code.extend([
                f";; Param {param}",
                "local.get $new_env_addr",
                f"i32.const {offset}",
                "i32.add",
                f"local.get {i + 1}",
                "f64.store"
            ])
        return "\n".join(code)

    def _emit_var_addr(self, info: SymbolInfo) -> str:
        base_ptr = "local.get $new_env_addr" if self.ctx.is_inside_func else "local.get $env\ni32.trunc_f64_u"
        code = [base_ptr]

        # Переход по ссылке (parent env) столько раз, сколько разница в уровнях вложенности
        hops = self.ctx.current_env.level - info.env_level
        for _ in range(hops):
            code.append("f64.load")  # Load parent ptr
            code.append("i32.trunc_f64_u")

        offset = (info.var_index + 1) * 8
        code.append(f"i32.const {offset}")
        code.append("i32.add")
        return "\n".join(code)

    # --- Visitor Methods ---

    def visit_defun(self, node: DefunNode):
        func_name = f"${node.name}"
        self.ctx.register_function(func_name)

        # Создаем окружение функции
        func_env = Environment(parent=self.global_env)
        for param in node.params:
            func_env.define(param)

        # Сохраняем текущее состояние и входим в функцию
        prev_env = self.ctx.current_env
        prev_is_inside = self.ctx.is_inside_func

        self.ctx.enter_function(func_env)

        body_wat = self._compile_block(node.body)
        prologue = self._alloc_frame(node.params, func_env.current_var_index + 10)

        # Возвращаем состояние
        self.ctx.exit_function(prev_env, prev_is_inside)

        params_wat = "(param $env f64) " + " ".join(["(param f64)" for _ in node.params])
        func_wat = f'  (func {func_name} {params_wat} (result f64)\n'
        func_wat += '    (local $new_env_addr i32)\n'
        func_wat += self._indent(prologue, 4) + '\n'
        func_wat += self._indent(body_wat, 4) + '\n'
        func_wat += '  )'

        self.ctx.funcs_code.append(func_wat)
        return ""

    def visit_lambda(self, node: LambdaNode) -> str:
        name = self.ctx.get_lambda_name()
        func_idx = self.ctx.register_function(name)

        lambda_env = node.closure_env

        prev_env = self.ctx.current_env
        prev_is_inside = self.ctx.is_inside_func
        self.ctx.enter_function(lambda_env)

        body_wat = self._compile_block(node.body)
        prologue = self._alloc_frame(node.params, lambda_env.current_var_index + 5)

        self.ctx.exit_function(prev_env, prev_is_inside)

        params_wat = "(param $env f64) " + " ".join(["(param f64)" for _ in node.params])
        func_wat = f'  (func {name} {params_wat} (result f64)\n'
        func_wat += '    (local $new_env_addr i32)\n'
        func_wat += self._indent(prologue, 4) + '\n'
        func_wat += self._indent(body_wat, 4) + '\n'
        func_wat += '  )'
        self.ctx.funcs_code.append(func_wat)

        # Создаем замыкание (пара cons: адрес функции + текущее окружение)
        env_ptr = "local.get $new_env_addr\nf64.convert_i32_u" if self.ctx.is_inside_func else "local.get $env"
        return f"f64.const {func_idx}.0\n{env_ptr}\ncall $std_cons"

    def visit_call(self, node: CallNode) -> str:
        # Делегируем примитивы отдельному классу
        if isinstance(node.func, SymbolNode) and node.func.name in Environment.PRIMITIVES:
            return self.visit_prim_call(PrimCallNode(node.func.name, node.args))

        # Логика косвенного вызова (call_indirect)
        func_calc = node.func.accept(self)
        args_calc = [arg.accept(self) for arg in node.args]
        arity = len(node.args)

        code = [
            f";; -- Call Indirect (arity {arity}) --",
            func_calc,
            "global.set $scratch",  # Сохраняем пару (func_idx . env)
            "global.get $scratch",
            "call $std_cdr",  # Извлекаем env для передачи первым параметром
        ]
        code.extend(args_calc)  # Параметры
        code.append("global.get $scratch")
        code.append("call $std_car")  # Извлекаем индекс функции
        code.append("i32.trunc_f64_u")
        code.append(f"call_indirect (type $type_{arity})")

        return "\n".join(code)

    def visit_prim_call(self, node: PrimCallNode) -> str:
        # Используем PrimitiveHandler для генерации кода
        return self.prim_handler.handle(node.prim_name, node.args, self)

    def visit_symbol(self, node: SymbolNode) -> str:
        # Разрешение переменных через Environment
        info = self.ctx.current_env.resolve(node.name)

        # Если переменная глобальная или не найдена (считаем глобальной)
        if not info or (info.env_level == 0 and info.var_index is None):
            return f"global.get ${node.name}"

        addr_code = self._emit_var_addr(info)
        return f"{addr_code}\nf64.load"

    def visit_setq(self, node: SetqNode) -> str:
        info = self.ctx.current_env.resolve(node.var_name)
        val_code = node.value.accept(self)

        if not info or (info.env_level == 0 and info.var_index is None):
            self.ctx.define_global(node.var_name)
            return f"{val_code}\nglobal.set ${node.var_name}\nglobal.get ${node.var_name}"

        code = f"{val_code}\n"
        code += "global.set $scratch\n"
        code += self._emit_var_addr(info) + "\n"
        code += "global.get $scratch\n"
        code += "f64.store\n"
        code += "global.get $scratch"
        return code

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
        # Рекурсивно создаем cons-ячейки для списка
        for elem in reversed(node.elements):
            elem_code = self._compile_quoted_element(elem)
            code = f"{elem_code}\n{code}\ncall $std_cons"
        return code

    def _compile_quoted_element(self, node: ASTNode):
        # Вспомогательный метод для обработки элементов внутри quote
        if isinstance(node, NumberNode): return f"f64.const {node.value}"
        if isinstance(node, ListNode): return self.visit_list(node)
        if isinstance(node, StringNode): return self.visit_string(node)
        return "f64.const 0.0"

    def visit_string(self, node: StringNode) -> str:
        # Строки хранятся в куче: [length: i32][byte...][byte]
        text_bytes = node.value.encode('utf-8')
        length = len(text_bytes)

        code = "global.get $heap_ptr\n"
        code += f"global.get $heap_ptr\ni32.const {length}\ni32.store\n"

        for i, byte in enumerate(text_bytes):
            code += f"global.get $heap_ptr\ni32.const {4 + i}\ni32.add\ni32.const {byte}\ni32.store8\n"

        code += f"global.get $heap_ptr\ni32.const {4 + length}\ni32.add\nglobal.set $heap_ptr\n"
        code += "f64.convert_i32_u"
        return code

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
        if node.op == 'and':
            return self._compile_and(node.args)
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