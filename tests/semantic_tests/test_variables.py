from semantic.ast_nodes import SetqNode, SymbolNode, LambdaNode
from semantic.diagnostics import ArityError, TypeMismatchError
from tests.semantic_tests.base_semantic_test import BaseSemanticTest


class TestVariables(BaseSemanticTest):
    def test_setq_simple(self):
        nodes, analyzer = self.parse_and_analyze("(setq x 42)")
        assert isinstance(nodes[0], SetqNode)
        assert nodes[0].var_name == "x"
        # Переменная должна попасть в глобальное окружение
        assert analyzer.global_env.resolve("x") is not None

    def test_symbol_resolution(self):
        """Чтение переменной после присваивания"""
        nodes, _ = self.parse_and_analyze("(setq x 42) x")
        assert isinstance(nodes[1], SymbolNode)
        assert nodes[1].name == "x"

    def test_scope_isolation(self):
        """Локальные переменные лямбды не видны снаружи"""
        code = "(lambda (x) (setq local 10))"
        _, analyzer = self.parse_and_analyze(code)
        assert analyzer.global_env.resolve("local") is None
        assert analyzer.global_env.resolve("x") is None

    def test_closure_captures_outer(self):
        """Замыкание захватывает внешние переменные"""
        code = """
        (setq x 10)
        (lambda (y) (+ x y))
        """
        nodes, _ = self.parse_and_analyze(code)
        lambda_node = nodes[1]
        assert isinstance(lambda_node, LambdaNode)
        assert lambda_node.closure_env.resolve("x") is not None

    def test_setq_errors(self):
        """Проверка ошибок setq: арность и типы"""
        #  Нет аргументов
        _, analyzer = self.parse_and_analyze("(setq)")
        assert isinstance(analyzer.collector.errors[0], ArityError)

        #  Первым аргументом число, а не символ
        self.setup_method()
        _, analyzer = self.parse_and_analyze("(setq 1 2)")
        assert isinstance(analyzer.collector.errors[0], TypeMismatchError)