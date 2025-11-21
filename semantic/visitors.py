"""
Конкретные реализации ASTVisitor
"""
from typing import List
from .ast_nodes import *
from .symbol_table import Environment


class RecursiveASTVisitor(ASTVisitor):
    """Рекурсивный обход AST без изменения состояния"""

    def visit_symbol(self, node: SymbolNode) -> SymbolNode:
        return node

    def visit_number(self, node: NumberNode) -> NumberNode:
        return node

    def visit_string(self, node: StringNode) -> StringNode:
        return node

    def visit_nil(self, node: NilNode) -> NilNode:
        return node

    def visit_true(self, node: TrueNode) -> TrueNode:
        return node

    def visit_quote(self, node: QuoteNode) -> QuoteNode:
        node.expr.accept(self)
        return node

    def visit_list(self, node: ListNode) -> ListNode:
        for elem in node.elements:
            elem.accept(self)
        return node

    def visit_call(self, node: CallNode) -> CallNode:
        node.func.accept(self)
        for arg in node.args:
            arg.accept(self)
        return node

    def visit_prim_call(self, node: PrimCallNode) -> PrimCallNode:
        for arg in node.args:
            arg.accept(self)
        return node

    def visit_setq(self, node: SetqNode) -> SetqNode:
        node.value.accept(self)
        return node

    def visit_cond(self, node: CondNode) -> CondNode:
        for pred, body in node.clauses:
            pred.accept(self)
            for expr in body:
                expr.accept(self)
        return node

    def visit_lambda(self, node: LambdaNode) -> LambdaNode:
        # Лямбду анализируем только один раз
        if not node.analyzed:
            old_env = getattr(self, 'current_env', None)
            self.current_env = node.closure_env
            for expr in node.body:
                expr.accept(self)
            if old_env:
                self.current_env = old_env
            node.analyzed = True
        return node


class WasmGenerator(RecursiveASTVisitor):
    """Генератор WASM (частичная реализация)"""

    def __init__(self):
        self.output = []
        self.func_count = 0

    def _emit(self, line: str):
        self.output.append(line)

    def generate(self, ast_nodes: List[ASTNode]) -> str:
        self._emit("(module")
        for node in ast_nodes:
            node.accept(self)
        self._emit(")")
        return "\n".join(self.output)