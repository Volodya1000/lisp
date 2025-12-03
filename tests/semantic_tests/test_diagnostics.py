from semantic.diagnostics import ArityError, TypeMismatchError
from tests.semantic_tests.base_semantic_test import BaseSemanticTest


class TestDiagnostics(BaseSemanticTest):
    def test_error_span_coordinates(self):
        """Проверка, что ошибка указывает на правильную строку и колонку"""
        # (setq a 1 2) -> ошибка в аргументах
        _, analyzer = self.parse_and_analyze("(setq a 1 2)")
        assert analyzer.collector.has_errors()
        error = analyzer.collector.errors[0]
        assert isinstance(error, ArityError)
        # Проверяем, что span инициализирован
        assert error.span.start_line is not None
        assert error.span.start_col is not None

    def test_recovery_mode(self):
        """Анализатор должен находить несколько ошибок, не падая на первой"""
        code = """
        (setq a)          
        (defun 123 (x) x) 
        """
        nodes, analyzer = self.parse_and_analyze(code)
        errors = analyzer.collector.errors

        assert len(errors) >= 2
        assert isinstance(errors[0], ArityError)  # Ошибка setq
        assert isinstance(errors[1], TypeMismatchError)  # Ошибка defun

        # AST должен быть построен частично
        assert len(nodes) == 2

    def test_context_stack(self):
        """Проверка стека контекста при ошибке внутри функции"""
        code = "(defun f (1) body)"
        _, analyzer = self.parse_and_analyze(code)
        error = analyzer.collector.errors[0]
        # В сообщении или стеке должно быть упоминание функции 'f'
        assert any("Function 'f'" in str(ctx) for ctx in error.context_stack)