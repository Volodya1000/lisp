from semantic import DefunNode, Environment, ASTVisitor, SymbolNode


class WasmCompiler(ASTVisitor):
    def __init__(self):
        # Глобальное окружение для функций и глобальных переменных
        self.global_env = Environment()
        # Текущее окружение (меняется при входе в функцию)
        self.current_env = self.global_env

        self.wat_output = []  # Список строк кода

    def visit_defun(self, node: DefunNode):
        # 1. Создаем новую область видимости для функции
        # Важно: parent=self.global_env, чтобы видеть глобальные функции
        func_env = Environment(parent=self.global_env)

        # Сохраняем старое окружение, переключаемся на новое
        previous_env = self.current_env
        self.current_env = func_env

        # 2. Регистрируем параметры как WASM locals (индексы 0, 1, 2...)
        # Это автоматически заполнит Map<String, Int> внутри func_env
        for param_name in node.params:
            idx = self.current_env.define_wasm_local(param_name)
            # idx здесь будет равен 0, потом 1, и т.д.

        # 3. Компилируем тело
        # Результат последнего выражения останется на стеке WASM
        for expr in node.body:
            expr.accept(self)

        # 4. Генерируем заголовок функции
        # (param f64) (param f64) ...
        params_wat = " ".join(["(param f64)" for _ in node.params])

        # Объявляем локальные переменные, которые не являются параметрами (если были setq/let)
        # (Это сложнее, но для MVP пока пропустим, считаем что locals = params)

        func_code = f'(func ${node.name} {params_wat} (result f64)\n'
        # Добавляем инструкции тела (нужно их собирать в буфер, здесь упрощенно)
        # ... тело ...
        func_code += ')\n'

        self.wat_output.append(func_code)

        # 5. Возвращаем старое окружение
        self.current_env = previous_env

    def visit_symbol(self, node: SymbolNode):
        # Ищем символ в текущем окружении
        info = self.current_env.resolve(node.name)

        if not info:
            raise RuntimeError(f"Compile error: Symbol '{node.name}' not found")

        # Логика генерации инструкций
        if info.wasm_loc_idx is not None:
            # Это локальная переменная (или аргумент)
            # Инструкция: local.get <индекс>
            self.emit(f'local.get {info.wasm_loc_idx}')
        elif info.env_level == 0:
            # Это глобальная переменная (если вы их поддерживаете)
            # Инструкция: global.get $<имя>
            self.emit(f'global.get ${info.name}')
        else:
            raise RuntimeError(f"Closure var '{node.name}' not supported yet")

    def emit(self, instruction: str):
        # Вспомогательный метод добавления инструкции в текущий буфер
        pass