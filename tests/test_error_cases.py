import pytest

from tests.base_semantic_test import BaseSemanticTest


class TestErrorCases(BaseSemanticTest):
    """Тесты ошибочных программ"""

    def test_quote_no_args(self):
        """(quote) без аргументов"""
        with pytest.raises(SyntaxError, match="quote требует 1 аргумент"):
            self.parse_and_analyze("(quote)")

    def test_lambda_no_args(self):
        """(lambda) без параметров и тела"""
        with pytest.raises(SyntaxError, match="lambda требует параметры и тело"):
            self.parse_and_analyze("(lambda)")

    def test_lambda_params_not_list(self):
        """Параметры не являются списком"""
        with pytest.raises(SyntaxError, match="Параметры lambda должны быть списком"):
            self.parse_and_analyze("(lambda x (+ x 1))")

    def test_lambda_param_not_symbol(self):
        """Параметр не является символом"""
        with pytest.raises(SyntaxError, match="Параметр.*должен быть символом"):
            self.parse_and_analyze("(lambda (1) x)")

    def test_setq_no_args(self):
        """(setq) без аргументов"""
        with pytest.raises(SyntaxError, match="setq требует 2 аргумента"):
            self.parse_and_analyze("(setq)")

    def test_setq_first_not_symbol(self):
        """Первый аргумент не символ"""
        with pytest.raises(SyntaxError, match="Первый аргумент setq должен быть символом"):
            self.parse_and_analyze("(setq 1 2)")

    def test_cond_empty_clause(self):
        """Cond с пустым clause"""
        with pytest.raises(SyntaxError, match="Неверный clause в cond"):
            self.parse_and_analyze("(cond ())")
