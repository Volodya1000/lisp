from typing import List, Set
from semantic.symbol_table import Environment


class CompilerContext:
    """
    Хранит изменяемое состояние процесса компиляции.
    """

    def __init__(self, global_env: Environment):
        self.global_env = global_env
        self.current_env = global_env

        # Код сгенерированных функций
        self.funcs_code: List[str] = []

        # Глобальные переменные (для секции (global ...))
        self.global_vars: Set[str] = set()

        # Таблица косвенных вызовов (call_indirect)
        self.table_entries: List[str] = []

        # Счетчик для уникальных имен лямбд
        self.lambda_counter = 0

        # Флаг: находимся ли мы внутри функции (влияет на доступ к переменным)
        self.is_inside_func = False

    def define_global(self, name: str):
        self.global_vars.add(name)
        self.global_env.define(name)

    def register_function(self, func_name: str):
        self.table_entries.append(func_name)
        return len(self.table_entries) - 1

    def get_lambda_name(self) -> str:
        name = f"$lambda_{self.lambda_counter}"
        self.lambda_counter += 1
        return name

    def enter_function(self, env: Environment):
        self.current_env = env
        self.is_inside_func = True

    def exit_function(self, prev_env: Environment, prev_state: bool):
        self.current_env = prev_env
        self.is_inside_func = prev_state