from typing import List, Set, Dict
from semantic.symbol_table import Environment
from .wasm_types import WasmType


class TypeRegistry:
    def __init__(self):
        # Хранит arity -> type_name
        self.registered_types: Set[int] = set()

    def get_or_register(self, arity: int) -> str:
        self.registered_types.add(arity)
        return f"$type_{arity}"

    def generate_definitions(self) -> str:
        lines = []
        for arity in sorted(self.registered_types):
            params = " ".join([f"(param {WasmType.F64})"] * (arity + 1))  # +1 for env
            lines.append(f"  (type $type_{arity} (func {params} (result {WasmType.F64})))")
        return "\n".join(lines)


class CompilerContext:
    def __init__(self, global_env: Environment):
        self.global_env = global_env
        self.current_env = global_env
        self.funcs_code: List[str] = []
        self.global_vars: Set[str] = set()
        self.table_entries: List[str] = []
        self.lambda_counter = 0
        self.is_inside_func = False

        # Новый реестр типов
        self.type_registry = TypeRegistry()

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

