"""
Unit-тесты для SemanticAnalyzer
"""
from antlr4 import InputStream, CommonTokenStream
from gen.lispLexer import lispLexer
from gen.lispParser import lispParser
from semantic.semantic_analyzer import SemanticAnalyzer


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
        return self.analyzer.visit(tree), self.analyzer




