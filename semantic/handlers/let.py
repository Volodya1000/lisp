from .base import SpecialFormHandler
from ..diagnostics import ArityError, TypeMismatchError, InvalidSyntaxError
from ..ast_nodes import ASTNode, NilNode, LambdaNode, CallNode, SymbolNode
from ..symbol_table import Environment


class LetHandler(SpecialFormHandler):
    def handle(self, analyzer, raw_args, ctx) -> ASTNode:
        # должен быть хотя бы список биндингов
        if len(raw_args) < 1:
            analyzer.collector.add_error(ArityError(
                message="let requires at least a bindings list",
                span=analyzer.get_span(ctx),
                func_name="let",
                expected_count=">= 1",
                actual_count=len(raw_args)
            ))
            return NilNode()

        bindings_ctx = raw_args[0]
        body_ctxs = raw_args[1:]

        params = []
        args_values = []

        # Проверка типа первого аргумента (список или nil)
        is_list = bindings_ctx.list_() is not None
        is_nil = bindings_ctx.atom() and bindings_ctx.atom().getText() == "nil"

        if not is_list and not is_nil:
            analyzer.collector.add_error(TypeMismatchError(
                message="let bindings must be a list",
                span=analyzer.get_span(bindings_ctx),
                expected_type="List",
                actual_type="Atom"
            ))
            # Не можем продолжать без биндингов, но чтобы не падать, возвращаем пустышку
            return NilNode()

        if is_list:
            # Итерируемся по парам (var val)
            raw_pairs = bindings_ctx.list_().sexpr()

            for i, pair_ctx in enumerate(raw_pairs):
                # Каждая пара должна быть списком
                if not pair_ctx.list_():
                    analyzer.collector.add_error(TypeMismatchError(
                        message=f"Binding #{i + 1} must be a list (var value)",
                        span=analyzer.get_span(pair_ctx),
                        expected_type="List",
                        actual_type="Atom"
                    ))
                    continue

                pair_elements = pair_ctx.list_().sexpr()

                # Структура должна быть (var val)
                if len(pair_elements) != 2:
                    analyzer.collector.add_error(InvalidSyntaxError(
                        message=f"Binding #{i + 1} has invalid structure",
                        span=analyzer.get_span(pair_ctx),
                        expected_syntax="(var value)"
                    ))
                    continue

                # Элемент 0: Имя переменной
                var_node = analyzer.visit(pair_elements[0])
                if not isinstance(var_node, SymbolNode):
                    analyzer.collector.add_error(TypeMismatchError(
                        message=f"Variable name in binding #{i + 1} must be a symbol",
                        span=analyzer.get_span(pair_elements[0]),
                        expected_type="Symbol",
                        actual_type=type(var_node).__name__
                    ))
                    continue

                # Элемент 1: Значение (компилируем в текущем окружении)
                val_node = analyzer.visit(pair_elements[1])

                params.append(var_node.name)
                args_values.append(val_node)

        #  Создание Scope и тела
        let_env = Environment(parent=analyzer.current_env)
        previous_env = analyzer.current_env
        analyzer.current_env = let_env

        body = []
        try:
            # Регистрируем переменные, чтобы тело их видело
            for p in params:
                analyzer.current_env.define(p)

            with analyzer.collector.context("Let Body"):
                # Если тело пустое, возвращаем nil, иначе компилируем список выражений
                if not body_ctxs:
                    body = [NilNode()]
                else:
                    body = [analyzer.visit(e) for e in body_ctxs]
        finally:
            analyzer.current_env = previous_env

        # переписываем let в немедленно вызываемую функцию
        lambda_node = LambdaNode(params, body, let_env)
        return CallNode(lambda_node, args_values)