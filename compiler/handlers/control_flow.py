from typing import List

from .base import BaseHandler
from ..wasm_types import WasmType, OpCode


class ControlFlowHandler(BaseHandler):
    def handle_if(self, condition, then_branch, else_branch, visitor) -> str:
        wb = self.get_builder()

        wb.raw(condition.accept(visitor))
        wb.emit_const('0.0')
        wb.emit(OpCode.NE, WasmType.F64)  # Проверка condition != 0.0 (True)

        with wb.if_block():
            wb.raw(then_branch.accept(visitor))
        with wb.else_block():
            if else_branch:
                wb.raw(else_branch.accept(visitor))
            else:
                wb.emit_const('0.0')  # Nil по умолчанию

        return wb.build()

    def handle_cond(self, clauses: List[tuple], visitor) -> str:
        """Рекурсивная генерация вложенных if-else для конструкции cond."""
        wb = self.get_builder()
        self._compile_cond_recursive(clauses, wb, visitor)
        return wb.build()

    def _compile_cond_recursive(self, clauses: List[tuple], wb, visitor):
        if not clauses:
            wb.emit_const('0.0')
            return

        pred, body = clauses[0]

        wb.raw(pred.accept(visitor))
        wb.emit_const('0.0')
        wb.emit(OpCode.NE, WasmType.F64)

        with wb.if_block():
            wb.raw(self.handle_progn(body, visitor))
        with wb.else_block():
            self._compile_cond_recursive(clauses[1:], wb, visitor)

    def handle_progn(self, body_nodes: List, visitor) -> str:
        if not body_nodes:
            return "f64.const 0.0"

        wb = self.get_builder()
        for i, node in enumerate(body_nodes):
            wb.raw(node.accept(visitor))
            # Если это не последнее выражение, сбрасываем его результат со стека
            if i < len(body_nodes) - 1:
                wb.emit(OpCode.DROP)
        return wb.build()

    def handle_logic(self, op: str, args: List, visitor) -> str:
        wb = self.get_builder()
        if op == 'and':
            self._compile_and(args, wb, visitor)
        else:
            self._compile_or(args, wb, visitor)
        return wb.build()

    def _compile_and(self, args, wb, visitor):
        if not args:
            wb.emit_const('1.0')  # (and) is True
            return

        curr, rest = args[0], args[1:]
        if not rest:
            wb.raw(curr.accept(visitor))
            return

        wb.raw(curr.accept(visitor))
        wb.emit_const('0.0')
        wb.emit(OpCode.NE, WasmType.F64)
        with wb.if_block():
            self._compile_and(rest, wb, visitor)
        with wb.else_block():
            wb.emit_const('0.0')

    def _compile_or(self, args, wb, visitor):
        if not args:
            wb.emit_const('0.0')  # (or) is False
            return

        curr, rest = args[0], args[1:]
        if not rest:
            wb.raw(curr.accept(visitor))
            return

        wb.raw(curr.accept(visitor))
        wb.emit_set('$scratch', 'global')  # Кэшируем результат вычисления
        wb.emit_get('$scratch', 'global')
        wb.emit_const('0.0')
        wb.emit(OpCode.NE, WasmType.F64)

        with wb.if_block():
            wb.emit_get('$scratch', 'global')
        with wb.else_block():
            self._compile_or(rest, wb, visitor)