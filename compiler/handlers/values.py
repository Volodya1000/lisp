from .base import BaseHandler
from ..wasm_types import WasmType, OpCode
from semantic.ast_nodes import NumberNode, StringNode, ListNode, QuoteNode


class ValueHandler(BaseHandler):

    def handle_number(self, node: NumberNode) -> str:
        return f"f64.const {node.value}"

    def handle_nil(self) -> str:
        return "f64.const 0.0"

    def handle_true(self) -> str:
        return "f64.const 1.0"

    def handle_string(self, node: StringNode) -> str:
        processed_val = node.value.replace('\\n', '\n').replace('\\t', '\t').replace('\\"', '"')
        text_bytes = processed_val.encode('utf-8')
        length = len(text_bytes)

        wb = self.get_builder()

        wb.emit_get('$heap_ptr', 'global')
        wb.emit_get('$heap_ptr', 'global')
        wb.emit_const(length, WasmType.I32)
        wb.emit(OpCode.STORE, WasmType.I32)

        for i, byte in enumerate(text_bytes):
            wb.emit_get('$heap_ptr', 'global')
            wb.emit_const(4 + i, WasmType.I32)
            wb.emit(OpCode.ADD, WasmType.I32)
            wb.emit_const(byte, WasmType.I32)
            wb.raw("i32.store8")

        wb.emit_get('$heap_ptr', 'global')
        wb.emit_const(4 + length, WasmType.I32)
        wb.emit(OpCode.ADD, WasmType.I32)
        wb.emit_set('$heap_ptr', 'global')

        wb.emit(OpCode.CONVERT_U, WasmType.F64)  # Исправлено
        return wb.build()

    def handle_quote(self, node: QuoteNode, visitor) -> str:
        if isinstance(node.expr, NumberNode): return self.handle_number(node.expr)
        if isinstance(node.expr, StringNode): return self.handle_string(node.expr)
        if isinstance(node.expr, ListNode): return self.handle_list(node.expr, visitor, quoted=True)
        return self.handle_nil()

    def handle_list(self, node: ListNode, visitor, quoted=False) -> str:
        code = "f64.const 0.0"
        for elem in reversed(node.elements):
            if quoted:
                from semantic.ast_nodes import QuoteNode
                elem_code = self.handle_quote(QuoteNode(elem), visitor)
            else:
                elem_code = elem.accept(visitor)

            code = f"{elem_code}\n{code}\ncall $std_cons"

        return code