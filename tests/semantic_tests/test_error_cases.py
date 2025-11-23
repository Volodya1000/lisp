import pytest
from tests.semantic_tests.base_semantic_test import BaseSemanticTest
from semantic.diagnostics import ArityError, TypeMismatchError, InvalidSyntaxError


class TestErrorCases(BaseSemanticTest):
    """
    Тесты ошибочных программ, адаптированные под Error Collector.
    Вместо pytest.raises(SyntaxError) проверяем содержимое analyzer.collector.errors.
    """

    def test_quote_no_args(self):
        """(quote) без аргументов"""
        # Old: quote требует 1 аргумент
        nodes, analyzer = self.parse_and_analyze("(quote)")

        assert analyzer.collector.has_errors()
        error = analyzer.collector.errors[0]

        assert isinstance(error, ArityError)
        assert error.func_name == "quote"
        assert error.actual_count == 0

    def test_lambda_no_args(self):
        """(lambda) без параметров и тела"""
        # Old: lambda требует параметры и тело
        nodes, analyzer = self.parse_and_analyze("(lambda)")

        assert analyzer.collector.has_errors()
        error = analyzer.collector.errors[0]

        assert isinstance(error, ArityError)
        assert error.func_name == "lambda"

    def test_lambda_params_not_list(self):
        """Параметры не являются списком"""
        # (lambda x ...) -> x - это символ, а нужен список
        nodes, analyzer = self.parse_and_analyze("(lambda x (+ x 1))")

        assert analyzer.collector.has_errors()
        error = analyzer.collector.errors[0]

        assert isinstance(error, TypeMismatchError)
        assert error.expected_type == "List"
        assert error.actual_type == "Symbol"

    def test_lambda_param_not_symbol(self):
        """Параметр не является символом"""
        # (lambda (1) x) -> 1 это число
        nodes, analyzer = self.parse_and_analyze("(lambda (1) x)")

        assert analyzer.collector.has_errors()
        error = analyzer.collector.errors[0]

        assert isinstance(error, TypeMismatchError)
        assert "must be a symbol" in error.message

    def test_setq_no_args(self):
        """(setq) без аргументов"""
        nodes, analyzer = self.parse_and_analyze("(setq)")

        assert analyzer.collector.has_errors()
        error = analyzer.collector.errors[0]

        assert isinstance(error, ArityError)
        assert error.func_name == "setq"
        assert error.expected_count == 2

    def test_setq_first_not_symbol(self):
        """Первый аргумент не символ"""
        nodes, analyzer = self.parse_and_analyze("(setq 1 2)")

        assert analyzer.collector.has_errors()
        error = analyzer.collector.errors[0]

        assert isinstance(error, TypeMismatchError)
        assert error.expected_type == "Symbol"
        assert error.actual_type == "NumberNode"

    def test_cond_invalid_clause(self):
        """
        Cond с невалидным clause.
        Раньше тестировалось (cond ()), но в новой реализации () парсится как NilNode
        и обрабатывается как false-ветка.
        Поэтому тестируем (cond 1), где clause - это атом.
        """
        nodes, analyzer = self.parse_and_analyze("(cond 1)")

        assert analyzer.collector.has_errors()
        error = analyzer.collector.errors[0]

        assert isinstance(error, InvalidSyntaxError)
        assert "Cond clause must be a list" in error.message