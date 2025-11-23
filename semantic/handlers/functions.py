from .base import SpecialFormHandler
from ..diagnostics import ArityError, TypeMismatchError

from ..ast_nodes import ASTNode, NilNode, LambdaNode, SymbolNode, DefunNode
from ..symbol_table import Environment


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