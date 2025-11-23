import pytest
from tests.semantic_tests.base_semantic_test import BaseSemanticTest
from semantic.diagnostics import ArityError, TypeMismatchError, InvalidSyntaxError
from semantic.ast_nodes import DefunNode


class TestStructuredErrors(BaseSemanticTest):
    """
    Тесты проверяют структуру ошибок, типы и recovery mode,
    а не просто строковые сообщения.
    """

    def test_arity_error_structure(self):
        """Проверка структуры ArityError и позиционирования (Span)."""
        code = "(setq a 1 2)"  # 3 аргумента вместо 2
        nodes, analyzer = self.parse_and_analyze(code)

        assert analyzer.collector.has_errors()
        errors = analyzer.collector.errors
        assert len(errors) == 1

        error = errors[0]
        assert isinstance(error, ArityError)
        assert error.func_name == "setq"
        assert error.expected_count == 2
        assert error.actual_count == 3

        # Проверка Span: строка 1, начинается с 0
        assert error.span.start_line == 1
        assert error.span.start_col == 0

    def test_type_mismatch_structure(self):
        """Проверка TypeMismatchError при неверном типе аргумента."""
        code = "(setq 123 456)"  # Первый аргумент должен быть символом
        nodes, analyzer = self.parse_and_analyze(code)

        assert len(analyzer.collector.errors) == 1
        error = analyzer.collector.errors[0]

        assert isinstance(error, TypeMismatchError)
        assert error.expected_type == "Symbol"
        assert error.actual_type == "NumberNode"

        # Контекст: (setq ...)
        # Span должен указывать на '123'
        assert error.span.start_col == 6  # "(setq 123..." (s=0, e=1, t=2, q=3, space=4, 1=6?) roughly

    def test_recovery_mode_multiple_errors(self):
        """
        Recovery Mode: анализатор должен найти ошибки в обоих выражениях,
        не останавливаясь на первом.
        """
        code = """
        (setq a)       
        (defun 123 (x) x) 
        """
        nodes, analyzer = self.parse_and_analyze(code)

        errors = analyzer.collector.errors
        assert len(errors) == 2

        # Ошибка 1: setq arity
        assert isinstance(errors[0], ArityError)
        assert errors[0].span.start_line == 2

        # Ошибка 2: defun name type mismatch
        assert isinstance(errors[1], TypeMismatchError)
        assert errors[1].span.start_line == 3

        # AST должен быть построен (хотя бы частично)
        assert len(nodes) == 2

    def test_context_stack_defun(self):
        """Проверка, что стек контекста правильно заполняется внутри функции."""
        code = "(defun my-func (10) body)"  # Параметр 10 невалиден
        nodes, analyzer = self.parse_and_analyze(code)

        error = analyzer.collector.errors[0]
        assert isinstance(error, TypeMismatchError)

        # Проверяем стек контекста
        # Должен содержать "Function 'my-func'"
        assert "Function 'my-func'" in error.context_stack
        assert error.context_stack[-1] == "Function 'my-func'"

    def test_cond_invalid_clause_structure(self):
        """Тест специальной ошибки синтаксиса cond."""
        code = "(cond t)"  # clause должен быть списком, а тут atom
        nodes, analyzer = self.parse_and_analyze(code)

        error = analyzer.collector.errors[0]
        assert isinstance(error, InvalidSyntaxError)
        assert "Cond clause must be a list" in error.message
        assert "Cond Clause #1" in error.context_stack

    def test_defun_valid_ast_despite_error(self):
        """
        Проверяем, что AST для defun создается даже если параметры кривые.
        Это нужно для работы IDE (Go to definition).
        """
        code = "(defun my-bad-func (a 1 b) x)"
        nodes, analyzer = self.parse_and_analyze(code)

        assert analyzer.collector.has_errors()  # Ошибка: 1 не символ

        defun_node = nodes[0]
        assert isinstance(defun_node, DefunNode)
        assert defun_node.name == "my-bad-func"
        # Валидные параметры должны остаться
        assert "a" in defun_node.params
        assert "b" in defun_node.params
        # Невалидный параметр исключен
        assert "1" not in defun_node.params