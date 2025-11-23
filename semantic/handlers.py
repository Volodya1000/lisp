from abc import ABC, abstractmethod
from .ast_nodes import *
from .symbol_table import Environment
from .diagnostics import ArityError, TypeMismatchError, InvalidSyntaxError
from .quote_builder import QuoteBuilder

if TYPE_CHECKING:
    from .analyzer import SemanticAnalyzer
    from gen.lispParser import lispParser


class SpecialFormHandler(ABC):
    """Базовый класс для обработки специальных форм."""

    @abstractmethod
    def handle(self, analyzer: 'SemanticAnalyzer', args: List['lispParser.SexprContext'], ctx) -> ASTNode:
        pass

    @staticmethod
    def _extract_params(analyzer: 'SemanticAnalyzer', params_ast: ASTNode, ctx_for_error) -> List[str]:
        """Утилита для извлечения параметров функций/лямбд."""
        # Убрана лишняя инициализация elements = []

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


# --- Concrete Handlers ---

class QuoteHandler(SpecialFormHandler):
    def handle(self, analyzer, raw_args, ctx) -> QuoteNode:
        if len(raw_args) != 1:
            analyzer.collector.add_error(ArityError(
                message=f"quote expects 1 argument, got {len(raw_args)}",
                span=analyzer.get_span(ctx),
                func_name="quote",
                expected_count=1,
                actual_count=len(raw_args)
            ))
            if raw_args:
                return QuoteNode(QuoteBuilder.build(raw_args[0]))
            return QuoteNode(NilNode())

        return QuoteNode(QuoteBuilder.build(raw_args[0]))


class SetqHandler(SpecialFormHandler):
    def handle(self, analyzer, raw_args, ctx) -> ASTNode:
        if len(raw_args) != 2:
            analyzer.collector.add_error(ArityError(
                message=f"setq expects 2 arguments, got {len(raw_args)}",
                span=analyzer.get_span(ctx),
                func_name="setq",
                expected_count=2,
                actual_count=len(raw_args)
            ))
            return NilNode()

        name_node = analyzer.visit(raw_args[0])
        var_name = "error_placeholder"

        if not isinstance(name_node, SymbolNode):
            analyzer.collector.add_error(TypeMismatchError(
                message=f"setq first argument must be a symbol, got {type(name_node).__name__}",
                span=analyzer.get_span(raw_args[0]),
                expected_type="Symbol",
                actual_type=type(name_node).__name__
            ))
        else:
            var_name = name_node.name

        value_node = analyzer.visit(raw_args[1])

        # Logic: Define if not exists
        existing = analyzer.current_env.resolve(var_name)
        if not existing:
            analyzer.current_env.define(var_name)

        return SetqNode(var_name, value_node)


class LambdaHandler(SpecialFormHandler):
    def handle(self, analyzer, raw_args, ctx) -> ASTNode:
        if len(raw_args) < 1:
            analyzer.collector.add_error(ArityError(
                message="lambda requires at least parameters list",
                span=analyzer.get_span(ctx),
                func_name="lambda",
                expected_count=">= 1",
                actual_count=len(raw_args)
            ))
            return NilNode()

        params_node = analyzer.visit(raw_args[0])
        params = self._extract_params(analyzer, params_node, raw_args[0])

        lambda_env = Environment(parent=analyzer.current_env)
        previous_env = analyzer.current_env
        analyzer.current_env = lambda_env

        body = []
        try:
            for p in params:
                analyzer.current_env.define(p)

            with analyzer.collector.context("Lambda Body"):
                body = [analyzer.visit(e) for e in raw_args[1:]]
        finally:
            analyzer.current_env = previous_env

        return LambdaNode(params, body, lambda_env)


class DefunHandler(SpecialFormHandler):
    def handle(self, analyzer, raw_args, ctx) -> ASTNode:
        if len(raw_args) < 3:
            analyzer.collector.add_error(ArityError(
                message="defun requires name, parameters and body",
                span=analyzer.get_span(ctx),
                func_name="defun",
                expected_count=">= 3",
                actual_count=len(raw_args)
            ))
            return NilNode()

        name_node = analyzer.visit(raw_args[0])
        func_name = "anonymous"

        if not isinstance(name_node, SymbolNode):
            analyzer.collector.add_error(TypeMismatchError(
                message="defun function name must be a symbol",
                span=analyzer.get_span(raw_args[0]),
                expected_type="Symbol",
                actual_type=type(name_node).__name__
            ))
        else:
            func_name = name_node.name
            analyzer.global_env.define(func_name, is_function=True)

        with analyzer.collector.context(f"Function '{func_name}'"):
            params_node = analyzer.visit(raw_args[1])
            params = self._extract_params(analyzer, params_node, raw_args[1])

            func_env = Environment(parent=analyzer.current_env)
            previous_env = analyzer.current_env
            analyzer.current_env = func_env

            body = []
            try:
                for p in params:
                    analyzer.current_env.define(p)
                body = [analyzer.visit(e) for e in raw_args[2:]]
            finally:
                analyzer.current_env = previous_env

        return DefunNode(func_name, params, body)


class CondHandler(SpecialFormHandler):
    def handle(self, analyzer, raw_args, ctx) -> ASTNode:
        clauses = []
        for i, clause_ctx in enumerate(raw_args):
            with analyzer.collector.context(f"Cond Clause #{i + 1}"):
                node = analyzer.visit(clause_ctx)

                # Убрана лишняя инициализация pred = None, body = []

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