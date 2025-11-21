"""
Генератор WebAssembly Text (WAT) из обогащенного AST
"""
from .ast_nodes import ASTVisitor, ListNode, TrueNode, NumberNode, NilNode, SymbolNode
from .symbol_table import Environment
from .ast_nodes import ASTVisitor, ASTNode


class WasmGenerator(ASTVisitor):
    """Третий проход: AST -> WAT"""

    def __init__(self):
        self.output = []
        self.indent_level = 0
        self.func_count = 0
        self.func_table = {}  # name -> index
        self.string_pool = {}  # string -> address

    def generate(self, ast_nodes: list) -> str:
        """Генерация полного модуля WASM"""
        self._emit("(module")
        self._indent()

        # Импортируем функции среды
        self._emit('(import "env" "print" (func $print (param i32)))')

        # Экспортируем память
        self._emit('(memory (export "memory") 1)')
        self._emit('(export "__heap_base" (global $heap_base))')
        self._emit('(global $heap_base (mut i32) (i32.const 1024))')

        # Генерируем все функции
        for node in ast_nodes:
            node.accept(self)

        self._dedent()
        self._emit(")")

        return "\n".join(self.output)

    def _emit(self, line: str):
        self.output.append("  " * self.indent_level + line)

    def _indent(self):
        self.indent_level += 1

    def _dedent(self):
        self.indent_level -= 1

    # --- Visitor методы ---

    def visit_symbol(self, node):
        # Символы компилируются в месте использования (в CallNode)
        pass

    def visit_number(self, node):
        self._emit(f"(i32.const {int(node.value)})")

    def visit_string(self, node):
        # Строки пулаются в память
        addr = self._allocate_string(node.value)
        self._emit(f"(i32.const {addr})")

    def visit_nil(self, node):
        self._emit("(i32.const 0)")  # nil = 0

    def visit_true(self, node):
        self._emit("(i32.const 1)")  # t = 1

    def visit_quote(self, node):
        # Quote компилируется в константу
        self._compile_constant(node.expr)

    def visit_lambda(self, node):
        # Генерируем уникальную функцию
        func_name = f"$lambda_{self.func_count}"
        self.func_count += 1

        # Регистрируем в таблице
        self.func_table[node] = len(self.func_table)

        self._emit(f"(func {func_name} (param $env i32) (result i32)")
        self._indent()

        # Компилируем тело
        for expr in node.body[:-1]:
            expr.accept(self)
            self._emit("drop")  # Отбрасываем промежуточные значения

        # Последнее выражение - результат
        node.body[-1].accept(self)

        self._dedent()
        self._emit(")")

        # Создаем замыкание: (func_index . env)
        closure_addr = self._allocate_closure(self.func_table[node], node.closure_env)
        self._emit(f"(i32.const {closure_addr})")

    def visit_call(self, node):
        # Компилируем функцию и аргументы
        node.func.accept(self)
        func_ptr = "$tmp_func"
        self._emit(f"local.set {func_ptr}")

        for arg in node.args:
            arg.accept(self)

        # Вызываем через call_indirect
        self._emit(f"local.get {func_ptr}")
        self._emit("i32.load offset=0")  # func_index
        self._emit("local.get {func_ptr}")
        self._emit("i32.load offset=4")  # env

        # call_indirect нужен тип функции
        self._emit("call_indirect (type $lisp_func)")

    def visit_prim_call(self, node):
        """Встроенные примитивы — прямые вызовы"""
        args = node.args

        if node.prim_name == 'cons':
            # cons(left, right) -> выделяет пару
            for arg in args:
                arg.accept(self)
            self._emit("call $prim_cons")
        elif node.prim_name == 'car':
            args[0].accept(self)
            self._emit("call $prim_car")
        elif node.prim_name == 'cdr':
            args[0].accept(self)
            self._emit("call $prim_cdr")
        elif node.prim_name == '+':
            # (+ a b c ...) -> складываем последовательно
            args[0].accept(self)
            for arg in args[1:]:
                arg.accept(self)
                self._emit("call $prim_add")
        elif node.prim_name == '-':
            if len(args) == 1:
                # Унарный минус
                args[0].accept(self)
                self._emit("call $prim_neg")
            else:
                # Бинарный минус
                args[0].accept(self)
                args[1].accept(self)
                self._emit("call $prim_sub")
        else:
            self._emit(f"call $prim_{node.prim_name}")

    def visit_setq(self, node):
        # Вычисляем значение
        node.value.accept(self)
        # Сохраняем в глобальную переменную (пока просто в память)
        var_addr = self._get_global_addr(node.var_name)
        self._emit(f"i32.store (i32.const {var_addr})")

    def visit_cond(self, node):
        # Генерируем цепочку if-else
        for i, (pred, body) in enumerate(node.clauses):
            # Компилируем предикат
            label_if = f"if_{i}"
            label_else = f"else_{i}"

            pred.accept(self)
            self._emit(f"if (result i32)")  # cond возвращает значение
            self._indent()

            # Тело
            for expr in body[:-1]:
                expr.accept(self)
                self._emit("drop")
            body[-1].accept(self)

            self._dedent()
            self._emit("else")
            self._indent()

        # Последний else -> nil
        self._emit("(i32.const 0)")

        # Закрываем все if
        for _ in node.clauses:
            self._dedent()
            self._emit("end")

    # --- Вспомогательные методы ---

    def _compile_constant(self, node: ASTNode):
        """Компиляция констант (для quote)"""
        if isinstance(node, SymbolNode):
            # Символы quote -> intern id
            self._emit(f"(i32.const {self._intern_symbol(node.name)})")
        elif isinstance(node, NumberNode):
            self._emit(f"(i32.const {int(node.value)})")
        elif isinstance(node, NilNode):
            self._emit("(i32.const 0)")
        elif isinstance(node, TrueNode):
            self._emit("(i32.const 1)")
        elif isinstance(node, ListNode):
            # Рекурсивно компилируем список
            if not node.elements:
                self._emit("(i32.const 0)")  # nil
            else:
                car = node.elements[0]
                cdr = ListNode(node.elements[1:]) if len(node.elements) > 1 else NilNode()
                self._compile_constant(car)
                self._compile_constant(cdr)
                self._emit("call $prim_cons")

    def _allocate_string(self, s: str) -> int:
        """Выделить строку в памяти, вернуть адрес"""
        if s in self.string_pool:
            return self.string_pool[s]

        # Выделяем память + константа для длины
        # Заглушка для примера
        addr = 2048 + len(self.string_pool) * 256
        self.string_pool[s] = addr
        return addr

    def _allocate_closure(self, func_idx: int, env: 'Environment') -> int:
        """Выделить пару (func_index . env)"""
        # Заглушка: просто возвращаем адрес
        return 4096 + func_idx * 8

    def _intern_symbol(self, name: str) -> int:
        """Интернировать символ, вернуть ID"""
        # Заглушка
        return hash(name) & 0xFFFF

    def _get_global_addr(self, var_name: str) -> int:
        """Получить адрес глобальной переменной"""
        # Заглушка: символы в памяти после 8192
        return 8192 + hash(var_name) % 1024