from typing import List, Set
from semantic.ast_nodes import *
from semantic.symbol_table import Environment


class WasmCompiler(ASTVisitor):
    def __init__(self):
        # Глобальное окружение для символов
        self.global_env = Environment()
        # Текущее окружение (начинаем с глобального)
        self.current_env = self.global_env

        # Хранилище сгенерированного кода функций (строки WAT)
        self.funcs_code: List[str] = []

        # Множество имен глобальных переменных для секции (global ...)
        self.global_vars: Set[str] = set()

    def compile(self, nodes: List[ASTNode]) -> str:
        """
        Главный метод: принимает список AST узлов, возвращает строку WAT (WebAssembly Text).
        """
        # 1. Предварительный проход: находим глобальные переменные и объявления функций
        self._scan_definitions(nodes)

        # Список узлов, которые пойдут в функцию main (все, что не defun)
        main_nodes: List[ASTNode] = []

        # 2. Разделяем узлы
        for node in nodes:
            if isinstance(node, DefunNode):
                # Код функции генерируется и сохраняется в self.funcs_code сразу
                node.accept(self)
            else:
                # Узлы верхнего уровня сохраняем как объекты, чтобы потом прогнать через _compile_body
                main_nodes.append(node)

        # 3. Сборка итогового модуля
        wat = '(module\n'

        # Импорт функции печати (для отладки/вывода)
        wat += '  (import "env" "print_number" (func $print_number (param f64)))\n'

        # Объявление глобальных переменных
        # (global $name (mut f64) (f64.const 0.0))
        for g_var in self.global_vars:
            wat += f'  (global ${g_var} (mut f64) (f64.const 0.0))\n'

        # Вставка скомпилированных функций
        for func_wat in self.funcs_code:
            wat += func_wat + '\n'

        # Генерация функции main
        wat += '  (func $main (export "main") (result f64)\n'

        # ИСПОЛЬЗУЕМ _compile_body, чтобы корректно обрабатывать стек (drop для промежуточных значений)
        if main_nodes:
            main_code = self._compile_body(main_nodes)
            # Добавляем отступы
            for line in main_code.split('\n'):
                wat += f'    {line}\n'
        else:
            # Если пусто, возвращаем 0.0
            wat += '    f64.const 0.0\n'

        wat += '  )\n'
        wat += ')'

        return wat

    def _scan_definitions(self, nodes: List[ASTNode]):
        """
        Проход 1: Регистрируем имена функций и глобальных переменных до генерации кода.
        Это нужно, чтобы функции могли вызывать друг друга, а глобальные переменные
        были объявлены в заголовке модуля.
        """
        for node in nodes:
            if isinstance(node, DefunNode):
                self.global_env.define(node.name, is_function=True)
            elif isinstance(node, SetqNode):
                # Если setq на верхнем уровне -> это глобальная переменная
                self.global_vars.add(node.var_name)
                self.global_env.define(node.var_name, is_function=False)

    # --- Visitor Methods: Функции ---

    def visit_defun(self, node: DefunNode):
        # 1. Создаем область видимости функции
        func_env = Environment(parent=self.global_env)
        previous_env = self.current_env
        self.current_env = func_env

        # 2. Регистрируем параметры как локальные переменные WASM (индексы 0, 1, ...)
        for param_name in node.params:
            self.current_env.define_wasm_local(param_name)

        # 3. Компилируем тело функции
        body_wat = self._compile_body(node.body)

        # 4. Восстанавливаем окружение
        self.current_env = previous_env

        # 5. Формируем WAT код
        params_wat = " ".join(["(param f64)" for _ in node.params])
        func_wat = f'  (func ${node.name} {params_wat} (result f64)\n'

        # Добавляем отступы для тела
        for line in body_wat.split('\n'):
            func_wat += f'    {line}\n'

        func_wat += '  )'

        self.funcs_code.append(func_wat)
        # defun в Lisp обычно возвращает имя функции, но здесь мы возвращаем пустую строку,
        # так как defun не генерирует код в поток выполнения main (он уходит в definitions).
        return ""

    def visit_call(self, node: CallNode) -> str:
        # Получаем имя вызываемой функции
        func_name = node.func.name if isinstance(node.func, SymbolNode) else None
        if not func_name:
            raise NotImplementedError("Indirect calls (calling a variable) not supported yet")

        # Компилируем аргументы
        args_code = [arg.accept(self) for arg in node.args]

        code = "\n    ".join(args_code)
        code += f"\n    call ${func_name}"
        return code

    # --- Visitor Methods: Переменные (Локальные и Глобальные) ---

    def visit_symbol(self, node: SymbolNode) -> str:
        info = self.current_env.resolve(node.name)
        if not info:
            raise RuntimeError(f"Символ не найден: {node.name}")

        if info.wasm_loc_idx is not None:
            # Это локальная переменная (аргумент функции)
            return f"local.get {info.wasm_loc_idx}"
        elif not info.is_function:
            # Это глобальная переменная
            if node.name not in self.global_vars:
                # Если переменная не была найдена при сканировании, регистрируем сейчас
                self.global_vars.add(node.name)
            return f"global.get ${node.name}"
        else:
            raise RuntimeError(f"Cannot use function name as variable directly: {node.name}")

    def visit_setq(self, node: SetqNode) -> str:
        # 1. Генерируем код для вычисления значения
        val_code = node.value.accept(self)

        # 2. Ищем переменную
        info = self.current_env.resolve(node.var_name)

        # Если переменной нет вообще -> создаем глобальную
        if not info:
            self.global_vars.add(node.var_name)
            info = self.global_env.define(node.var_name)

        if info.wasm_loc_idx is not None:
            # Локальная переменная: используем local.tee
            # (записывает в переменную и оставляет значение на стеке)
            return f"{val_code}\n    local.tee {info.wasm_loc_idx}"
        else:
            # Глобальная переменная
            # global.set забирает значение со стека.
            # Чтобы вернуть значение (как делает setq), нужно прочитать его обратно.
            return f"{val_code}\n    global.set ${node.var_name}\n    global.get ${node.var_name}"

    # --- Visitor Methods: Базовые типы ---

    def visit_number(self, node: NumberNode) -> str:
        return f"f64.const {node.value}"

    def visit_nil(self, node: NilNode) -> str:
        return "f64.const 0.0"

    def visit_true(self, node: TrueNode) -> str:
        return "f64.const 1.0"

    # --- Visitor Methods: Примитивы и Математика ---

    def visit_prim_call(self, node: PrimCallNode) -> str:
        op_map = {
            '+': 'f64.add',
            '-': 'f64.sub',
            '*': 'f64.mul',
            '/': 'f64.div',
            '=': 'f64.eq',
            '<': 'f64.lt',
            '>': 'f64.gt',
            '<=': 'f64.le',
            '>=': 'f64.ge'
        }

        # Специальная обработка print
        if node.prim_name == 'print':
            if len(node.args) != 1:
                raise RuntimeError("print takes exactly 1 argument")
            val = node.args[0].accept(self)
            # Вызываем импортированную функцию и возвращаем 0.0 (nil)
            return f"{val}\n    call $print_number\n    f64.const 0.0"

        # Специальная обработка not
        if node.prim_name == 'not':
            if len(node.args) != 1:
                raise RuntimeError("not takes exactly 1 argument")
            val = node.args[0].accept(self)
            # Если val == 0.0, возвращаем 1. Иначе 0.
            return f"{val}\n    f64.const 0.0\n    f64.eq\n    f64.convert_i32_s"

        if node.prim_name not in op_map:
            raise NotImplementedError(f"Primitive '{node.prim_name}' not supported in WASM")

        # Компиляция аргументов
        args_code = [arg.accept(self) for arg in node.args]
        code = "\n    ".join(args_code)

        # Добавление операции
        op = op_map[node.prim_name]
        code += f"\n    {op}"

        # Конвертация результата сравнения (i32 -> f64)
        if node.prim_name in ['=', '<', '>', '<=', '>=']:
            code += "\n    f64.convert_i32_s"

        return code

    # --- Visitor Methods: Управление потоком (Cond) ---

    def visit_cond(self, node: CondNode) -> str:
        if not node.clauses:
            return "f64.const 0.0"

        # Берем первый clause
        pred_node, body_nodes = node.clauses[0]

        # Оптимизация для 't' (else ветка)
        is_always_true = isinstance(pred_node, TrueNode) or (
                isinstance(pred_node, SymbolNode) and pred_node.name == 't')

        if is_always_true:
            return self._compile_body(body_nodes)

        # Компилируем предикат
        code = pred_node.accept(self)

        # Проверка условия: (val != 0.0) -> i32
        code += "\n    f64.const 0.0\n    f64.ne"

        # if (result f64) ...
        code += "\n    (if (result f64)"

        # THEN block
        code += "\n      (then"
        code += "\n        " + self._compile_body(body_nodes).replace("\n", "\n        ")
        code += "\n      )"

        # ELSE block
        code += "\n      (else"
        rest_clauses = node.clauses[1:]
        if rest_clauses:
            # Рекурсивно обрабатываем остальные ветки
            else_code = self.visit_cond(CondNode(rest_clauses))
            code += "\n        " + else_code.replace("\n", "\n        ")
        else:
            code += "\n        f64.const 0.0"
        code += "\n      )"

        code += "\n    )"
        return code

    # --- Helper: Компиляция тела (последовательность выражений) ---

    def _compile_body(self, nodes: List[ASTNode]) -> str:
        """
        Компилирует список выражений. Результат всех выражений, кроме последнего,
        удаляется со стека (instruction `drop`).
        """
        if not nodes:
            return "f64.const 0.0"

        lines = []
        for i in range(len(nodes) - 1):
            lines.append(nodes[i].accept(self))
            lines.append("    drop")  # Значение промежуточного выражения не нужно

        # Последнее выражение оставляем на стеке как результат
        lines.append(nodes[-1].accept(self))

        return "\n".join(lines)

    # --- Заглушки для неподдерживаемых функций ---

    def visit_string(self, node):
        raise NotImplementedError("Strings are not supported in WASM MVP")

    def visit_quote(self, node):
        raise NotImplementedError("Quote is not supported in WASM MVP (requires heap)")

    def visit_list(self, node):
        raise NotImplementedError("Lists are not supported in WASM MVP (requires heap)")

    def visit_lambda(self, node):
        raise NotImplementedError("Lambdas are not supported in WASM MVP")