from .base import SpecialFormHandler
from ..diagnostics import ArityError, TypeMismatchError
from ..quote_builder import QuoteBuilder

from ..ast_nodes import ASTNode, NilNode, QuoteNode, SetqNode, SymbolNode


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

        # проверка вдруг уже существует
        existing = analyzer.current_env.resolve(var_name)
        if not existing:
            analyzer.current_env.define(var_name)

        return SetqNode(var_name, value_node)