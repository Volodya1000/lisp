from abc import ABC, abstractmethod
from typing import List, TYPE_CHECKING

from ..ast_nodes import ASTNode, ListNode, CallNode, PrimCallNode, NilNode, SymbolNode
from ..diagnostics import  TypeMismatchError

if TYPE_CHECKING:
    from ..semantic_analyzer import SemanticAnalyzer
    from gen.lispParser import lispParser


class SpecialFormHandler(ABC):

    @abstractmethod
    def handle(self, analyzer: 'SemanticAnalyzer', args: List['lispParser.SexprContext'], ctx) -> ASTNode:
        pass

    @staticmethod
    def _extract_params(analyzer: 'SemanticAnalyzer', params_ast: ASTNode, ctx_for_error) -> List[str]:
        """извлечение параметров функций/лямбд."""

        if isinstance(params_ast, ListNode):
            elements = params_ast.elements
        elif isinstance(params_ast, CallNode):
            elements = [params_ast.func] + params_ast.args
        elif isinstance(params_ast, PrimCallNode):
            elements = [SymbolNode(params_ast.prim_name)] + params_ast.args
        elif isinstance(params_ast, NilNode):
            elements = []
        elif isinstance(params_ast, SymbolNode):
            analyzer.collector.add_error(TypeMismatchError(
                message="Parameters must be a list",
                span=analyzer.get_span(ctx_for_error),
                expected_type="List",
                actual_type="Symbol"
            ))
            return []
        else:
            analyzer.collector.add_error(TypeMismatchError(
                message="Parameters must be a list",
                span=analyzer.get_span(ctx_for_error),
                expected_type="List",
                actual_type=type(params_ast).__name__
            ))
            return []

        params = []
        for i, p in enumerate(elements):
            if isinstance(p, SymbolNode):
                params.append(p.name)
            else:
                analyzer.collector.add_error(TypeMismatchError(
                    message=f"Parameter #{i + 1} must be a symbol",
                    span=analyzer.get_span(ctx_for_error),
                    expected_type="Symbol",
                    actual_type=type(p).__name__
                ))
        return params