from .base import SpecialFormHandler
from ..diagnostics import InvalidSyntaxError

from ..ast_nodes import ASTNode, NilNode, ListNode, CallNode, PrimCallNode, CondNode, PrognNode, LogicNode


class CondHandler(SpecialFormHandler):
    def handle(self, analyzer, raw_args, ctx) -> ASTNode:
        clauses = []
        for i, clause_ctx in enumerate(raw_args):
            with analyzer.collector.context(f"Cond Clause #{i + 1}"):
                node = analyzer.visit(clause_ctx)

                pred = None
                body = []

                if isinstance(node, ListNode):
                    if not node.elements:
                        pred = NilNode()
                        body = [NilNode()]
                    else:
                        pred = node.elements[0]
                        body = node.elements[1:]
                elif isinstance(node, CallNode):
                    pred = node.func
                    body = node.args
                elif isinstance(node, PrimCallNode):
                    pred = node
                    body = []
                elif isinstance(node, NilNode):
                    pred = NilNode()
                    body = [NilNode()]
                else:
                    analyzer.collector.add_error(InvalidSyntaxError(
                        message="Cond clause must be a list",
                        span=analyzer.get_span(clause_ctx),
                        expected_syntax="(predicate body...)"
                    ))
                    pred = node
                    body = [NilNode()]

                if not body:
                    body = [NilNode()]

                clauses.append((pred, body))
        return CondNode(clauses)


class PrognHandler(SpecialFormHandler):
    def handle(self, analyzer, raw_args, ctx) -> ASTNode:
        body = [analyzer.visit(expr) for expr in raw_args]
        return PrognNode(body)


class LogicHandler(SpecialFormHandler):
    def __init__(self, op: str):
        self.op = op

    def handle(self, analyzer, raw_args, ctx) -> ASTNode:
        args = [analyzer.visit(expr) for expr in raw_args]
        return LogicNode(self.op, args)