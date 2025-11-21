"""
Unit-тесты для SemanticAnalyzer
"""
import pytest
from antlr4 import InputStream, CommonTokenStream
from gen.lispLexer import lispLexer
from gen.lispParser import lispParser
from semantic.semantic_analyzer import SemanticAnalyzer
from semantic.ast_nodes import *


class BaseSemanticTest:
    """Базовый класс для всех тестов семантического анализатора"""

    def setup_method(self):
        """Вызывается перед каждым тестом"""
        self.analyzer = SemanticAnalyzer()

    def parse_and_analyze(self, code: str):
        """Вспомогательный метод: парсит и анализирует код"""
        input_stream = InputStream(code)
        lexer = lispLexer(input_stream)
        tokens = CommonTokenStream(lexer)
        parser = lispParser(tokens)
        tree = parser.program()
        return self.analyzer.visit(tree)




class TestEdgeCases(BaseSemanticTest):
    """Граничные случаи"""

    def test_very_nested_quote(self):
        """Глубоко вложенный quote"""
        ast = self.parse_and_analyze("'''a")
        assert isinstance(ast[0], QuoteNode)
        assert isinstance(ast[0].expr, QuoteNode)
        assert isinstance(ast[0].expr.expr, QuoteNode)

    def test_lambda_empty_params(self):
        """Лямбда с пустыми параметрами"""
        ast = self.parse_and_analyze("(lambda () 42)")
        assert isinstance(ast[0], LambdaNode)
        assert ast[0].params == []

    def test_complex_expression(self):
        """Сложное выражение"""
        code = """
        (setq add (lambda (x y) (+ x y)))
        (setq result (add 10 20))
        (cond 
          ((eq result 30) 'correct)
          (t 'wrong))
        """
        ast = self.parse_and_analyze(code)
        assert len(ast) == 3
        assert isinstance(ast[0], SetqNode)
        assert isinstance(ast[1], SetqNode)
        assert isinstance(ast[2], CondNode)