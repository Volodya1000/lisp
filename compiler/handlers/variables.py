import logging
from .base import BaseHandler
from ..wasm_types import WasmType, OpCode
from ..frame_policy import FramePolicy
from semantic.symbol_table import Environment

logger = logging.getLogger(__name__)


class VariableHandler(BaseHandler):
    def handle_get(self, name: str) -> str:
        """Обработка получения значения переменной (SymbolNode)."""
        wb = self.get_builder()

        #  Проверяем локальные/замыкаемые переменные
        info = self.ctx.current_env.resolve(name)
        if info and info.env_level > 0:
            FramePolicy.emit_var_address(
                wb,
                self.ctx.current_env.level,
                info.env_level,
                info.var_index,
                self.ctx.is_inside_func
            )
            wb.emit(OpCode.LOAD, WasmType.F64)
            return wb.build()

        #  Проверяем глобальные переменные
        if name in self.ctx.global_vars:
            wb.emit_get(f"${name}", 'global')
            return wb.build()

        if name in Environment.PRIMITIVES:
            raise Exception(f"Compiler Error: Cannot use primitive '{name}' as a value directly.")

        raise Exception(f"Compiler Error: Undefined variable '{name}'")

    def handle_set(self, name: str, value_node, compiler_visitor) -> str:
        """Обработка присваивания (SetqNode)."""
        wb = self.get_builder()

        # Компилируем вычисление значения
        val_code = value_node.accept(compiler_visitor)

        info = self.ctx.current_env.resolve(name)

        #  Присваивание в локальную/замыкаемую переменную
        if info and info.env_level > 0:
            wb.raw(val_code)
            wb.emit_set('$scratch', 'global')  # Сохраняем значение во временный регистр

            # Вычисляем адрес
            FramePolicy.emit_var_address(
                wb,
                self.ctx.current_env.level,
                info.env_level,
                info.var_index,
                self.ctx.is_inside_func
            )

            # Записываем значение по адресу
            wb.emit_get('$scratch', 'global')
            wb.emit(OpCode.STORE, WasmType.F64)

            # Возвращаем результат присваивания
            wb.emit_get('$scratch', 'global')
            return wb.build()

        # 2. Присваивание в глобальную переменную (или создание новой)
        if name not in self.ctx.global_vars:
            self.ctx.define_global(name)

        wb.raw(val_code)
        wb.emit_set(f"${name}", 'global')
        wb.emit_get(f"${name}", 'global')
        return wb.build()