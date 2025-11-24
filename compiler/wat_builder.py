from contextlib import contextmanager
from typing import List, Optional

from compiler.wasm_types import WasmType


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
