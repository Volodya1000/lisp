import sys
from pathlib import Path
from antlr4 import InputStream, CommonTokenStream

ROOT = Path(__file__).parent.resolve()
sys.path.insert(0, str(ROOT))

from gen.lispLexer import lispLexer
from gen.lispParser import lispParser
from semantic.semantic_analyzer import SemanticAnalyzer


def parse_and_analyze(file_path: Path):
    """Общий пайплайн парсинга и семантического анализа"""
    if not file_path.exists():
        raise FileNotFoundError(f"File '{file_path}' not found.")

    code = file_path.read_text(encoding='utf-8')

    input_stream = InputStream(code)
    lexer = lispLexer(input_stream)
    stream = CommonTokenStream(lexer)
    parser = lispParser(stream)
    tree = parser.program()

    if parser.getNumberOfSyntaxErrors() > 0:
        raise SyntaxError("Parsing failed.")

    analyzer = SemanticAnalyzer()
    ast = analyzer.visit(tree)

    if analyzer.collector.has_errors():
        errors = "\n".join(f"  - {err}" for err in analyzer.collector.errors)
        raise RuntimeError(f"Semantic Errors found:\n{errors}")

    return ast