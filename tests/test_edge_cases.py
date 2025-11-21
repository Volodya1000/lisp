from semantic.ast_nodes import *
from tests.base_semantic_test import BaseSemanticTest

class TestEdgeCases(BaseSemanticTest):
    """Граничные случаи"""

    def test_very_nested_quote(self):
        """Глубоко вложенный quote"""
        nodes, _ = self.parse_and_analyze("'''a")
        assert isinstance(nodes[0], QuoteNode)
        # quote -> quote -> quote -> symbol(a)
        assert isinstance(nodes[0].expr, QuoteNode)

    def test_lambda_empty_params(self):
        """Лямбда с пустыми параметрами"""
        nodes, _ = self.parse_and_analyze("(lambda () 42)")
        assert isinstance(nodes[0], LambdaNode)
        assert nodes[0].params == []

    def test_complex_expression(self):
        """Сложное выражение"""
        code = """
        (setq add (lambda (x y) (+ x y)))
        (setq result (add 10 20))
        (cond 
          ((eq result 30) 'correct)
          (t 'wrong))
        """
        nodes, _ = self.parse_and_analyze(code)
        assert len(nodes) == 3