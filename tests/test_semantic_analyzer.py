import pytest
from antlr4 import InputStream, CommonTokenStream
import gen.lisp_grammerLexer as LexerModule
import gen.lisp_grammerParser as ParserModule
from semantic.semantic import SemanticAnalyzer


class TestSemanticAnalyzer:
    """Набор тестов для проверки семантического анализатора."""

    def analyze(self, code: str):
        """Проводит анализ Lisp-кода и возвращает список ошибок."""
        input_stream = InputStream(code)
        lexer = LexerModule.lisp_grammerLexer(input_stream)
        stream = CommonTokenStream(lexer)
        parser = ParserModule.lisp_grammerParser(stream)
        tree = parser.program()
        analyzer = SemanticAnalyzer()
        errors = analyzer.analyze(tree)
        return errors

    @staticmethod
    def msg_contains(errors, substring):
        """Проверяет, содержится ли подстрока в сообщениях ошибок."""
        return any(substring in e.message for e in errors)

    def test_name_error(self):
        errors = self.analyze("(let ((x 1)) y)")
        assert self.msg_contains(errors, "Undefined")

    def test_duplicate_var_in_let(self):
        errors = self.analyze("(let ((x 1) (x 2)) (+ x x))")
        assert self.msg_contains(errors, "Duplicate binding")

    def test_attempt_to_call_non_function(self):
        errors = self.analyze("(42 1 2)")
        assert self.msg_contains(errors, "Attempt to call")

    def test_lambda_wrong_arity(self):
        errors = self.analyze("((lambda (x y) (+ x y)) 1)")
        assert self.msg_contains(errors, "expects")

    def test_duplicate_lambda_params(self):
        errors = self.analyze("(lambda (x x) (+ x 1))")
        assert self.msg_contains(errors, "Duplicate parameter")

    def test_division_by_zero_literal(self):
        errors = self.analyze("(/ 4 0)")
        assert self.msg_contains(errors, "Division by zero")
