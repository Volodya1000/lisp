from antlr4 import InputStream, CommonTokenStream
from gen.lispLexer import lispLexer
from gen.lispParser import lispParser
from semantic.semantic_analyzer import SemanticAnalyzer

class BaseSemanticTest:
    def setup_method(self):
        self.analyzer = SemanticAnalyzer()

    def parse_and_analyze(self, code: str):
        input_stream = InputStream(code)
        lexer = lispLexer(input_stream)
        tokens = CommonTokenStream(lexer)
        parser = lispParser(tokens)

        # Парсинг
        tree = parser.program()

        nodes = self.analyzer.visit(tree)

        return nodes, self.analyzer


