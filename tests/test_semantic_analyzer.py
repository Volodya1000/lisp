import pytest
from antlr4 import InputStream, CommonTokenStream
import gen.lisp_grammerLexer as LexerModule
import gen.lisp_grammerParser as ParserModule
from semantic.errors import (
    NameErrorSemantic,
    DuplicateDeclarationError,
    AttemptToCallNonFunctionError,
    ArityError,
    DivisionByZeroError,
)
from semantic.semantic_analizer import SemanticAnalyzer

class TestSemanticAnalyzer:
    """Тесты семантического анализатора Lisp."""

    def analyze(self, code: str):
        input_stream = InputStream(code)
        lexer = LexerModule.lisp_grammerLexer(input_stream)
        stream = CommonTokenStream(lexer)
        parser = ParserModule.lisp_grammerParser(stream)
        tree = parser.program()
        analyzer = SemanticAnalyzer()
        return analyzer.analyze(tree)

    @staticmethod
    def has_error(errors, error_type):
        return any(isinstance(e, error_type) for e in errors)

    @staticmethod
    def msg_contains(errors, substring):
        return any(substring in e.message for e in errors)

    # ===================== Ошибочные примеры =====================
    @pytest.mark.parametrize("code, error_type, substring", [
        ("(let ((x 1)) y)", NameErrorSemantic, "Undefined"),
        ("(let ((x 1) (x 2)) (+ x x))", DuplicateDeclarationError, "Duplicate binding"),
        ("(42 1 2)", AttemptToCallNonFunctionError, "Attempt to call"),
        ("((lambda (x y) (+ x y)) 1)", ArityError, "expects"),
        ("(lambda (x x) (+ x 1))", DuplicateDeclarationError, "Duplicate parameter"),
        ("(/ 4 0)", DivisionByZeroError, "Division by zero"),
    ])
    def test_semantic_errors(self, code, error_type, substring):
        errors = self.analyze(code)
        assert self.has_error(errors, error_type), (
            f"Expected {error_type.__name__}, got {[type(e).__name__ for e in errors]}"
        )
        assert self.msg_contains(errors, substring), (
            f"Message does not contain '{substring}'. Messages: {[e.message for e in errors]}"
        )

    # ===================== Корректные примеры =====================
    @pytest.mark.parametrize("code", [
        # 1. let с одной переменной, корректное использование
        "(let ((x 10)) (+ x 5))",

        # 2. Lambda с правильным количеством аргументов
        "((lambda (a b) (+ a b)) 3 4)",

        # 3. Вложенные let
        "(let ((x 2)) (let ((y 3)) (+ x y)))",

        # 4. Вызов встроенной функции с правильной арностью
        "(+ 1 2 3 4)",

        # 5. Lambda внутри let, без ошибок
        "(let ((f (lambda (x) (* x x)))) (f 5))",
    ])
    def test_no_semantic_errors(self, code):
        errors = self.analyze(code)
        assert len(errors) == 0, f"Expected no errors, got {[e.message for e in errors]}"