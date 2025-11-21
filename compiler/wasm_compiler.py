from typing import List
from semantic.ast_nodes import *
from semantic.symbol_table import Environment


class WasmCompiler(ASTVisitor):
    def __init__(self):
        # Глобальное окружение
        self.global_env = Environment()
        # Текущее окружение (на старте - глобальное)
        self.current_env = self.global_env

        # Сгенерированный код функций (список строк WAT)
        self.funcs_code: List[str] = []

    def compile(self, nodes: List[ASTNode]) -> str:
        main_body_code = []

        # 1. Сначала регистрируем все глобальные функции в окружении,
        # чтобы function A могла вызвать function B, определенную ниже.
        for node in nodes:
            if isinstance(node, DefunNode):
                self.global_env.define(node.name, is_function=True)

        # 2. Компилируем узлы
        for node in nodes:
            if isinstance(node, DefunNode):
                # defun сам добавит свой код в self.funcs_code
                node.accept(self)
            else:
                # Это выражение верхнего уровня -> идет в main
                main_body_code.append(node.accept(self))

        # 3. Собираем модуль
        wat = '(module\n'

        # Добавляем сгенерированные функции
        for func_wat in self.funcs_code:
            wat += func_wat + '\n'

        # Добавляем функцию main
        wat += '  (func $main (export "main") (result f64)\n'
        if main_body_code:
            for line in main_body_code:
                wat += f'    {line}\n'
        else:
            # Если main пустой, возвращаем 0.0 (чтобы валидатор не ругался на result f64)
            wat += '    f64.const 0.0\n'

        wat += '  )\n'
        wat += ')'
        return wat

    # --- Новые методы ---

    def visit_defun(self, node: DefunNode):
        # 1. Создаем новый Scope (Parent = Global)
        func_env = Environment(parent=self.global_env)

        # Переключаем контекст
        previous_env = self.current_env
        self.current_env = func_env

        # 2. Регистрируем параметры как локальные переменные (idx 0, 1...)
        # Это создаст записи в func_env с wasm_loc_idx
        for param_name in node.params:
            self.current_env.define_wasm_local(param_name)

        # 3. Компилируем тело функции
        body_code = []
        for expr in node.body:
            body_code.append(expr.accept(self))

        # 4. Восстанавливаем контекст
        self.current_env = previous_env

        # 5. Формируем WAT код функции
        # (func $name (param f64)... (result f64) ...body...)
        params_wat = " ".join(["(param f64)" for _ in node.params])

        func_wat = f'  (func ${node.name} {params_wat} (result f64)\n'
        func_wat += "\n".join([f'    {line}' for line in body_code])
        func_wat += '\n  )'

        self.funcs_code.append(func_wat)
        return ""  # defun ничего не возвращает в main flow

    def visit_symbol(self, node: SymbolNode) -> str:
        info = self.current_env.resolve(node.name)
        if not info:
            raise RuntimeError(f"Символ не найден: {node.name}")

        if info.wasm_loc_idx is not None:
            # Это локальная переменная (аргумент функции)
            return f"local.get {info.wasm_loc_idx}"
        else:
            # Пока не поддерживаем глобальные переменные
            raise NotImplementedError(f"Глобальные переменные или замыкания пока не поддерживаются: {node.name}")

    def visit_call(self, node: CallNode) -> str:
        # Вызов пользовательской функции: (myfunc 1 2)

        # 1. Проверяем, существует ли функция (в глобальном env)
        func_name = node.func.name if isinstance(node.func, SymbolNode) else None
        if not func_name:
            raise NotImplementedError("Indirect calls not supported yet")

        # 2. Компилируем аргументы
        args_code = []
        for arg in node.args:
            args_code.append(arg.accept(self))

        # 3. Генерируем вызов
        code = "\n    ".join(args_code)
        code += f"\n    call ${func_name}"
        return code

    # --- Старые методы (без изменений) ---

    def visit_number(self, node: NumberNode) -> str:
        return f"f64.const {node.value}"

    def visit_prim_call(self, node: PrimCallNode) -> str:
        op_map = {'+': 'f64.add', '-': 'f64.sub', '*': 'f64.mul', '/': 'f64.div'}
        if node.prim_name not in op_map:
            raise NotImplementedError(f"Primitive '{node.prim_name}' not supported")

        args_code = [arg.accept(self) for arg in node.args]
        return "\n    ".join(args_code) + f"\n    {op_map[node.prim_name]}"

    # Заглушки
    def visit_string(self, node):
        raise NotImplementedError()

    def visit_nil(self, node):
        raise NotImplementedError()

    def visit_true(self, node):
        raise NotImplementedError()

    def visit_quote(self, node):
        raise NotImplementedError()

    def visit_list(self, node):
        raise NotImplementedError()

    def visit_lambda(self, node):
        raise NotImplementedError()

    def visit_setq(self, node):
        raise NotImplementedError()

    def visit_cond(self, node):
        raise NotImplementedError()