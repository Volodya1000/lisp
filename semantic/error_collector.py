from typing import List, Optional
from contextlib import contextmanager
from .diagnostics import SemanticError

class ErrorCollector:
    def __init__(self):
        self._errors: List[SemanticError] = []
        self._context_stack: List[str] = []

    def add_error(self, error: SemanticError):
        # Копируем текущий стек контекста в ошибку
        error.context_stack = list(self._context_stack)
        self._errors.append(error)

    def has_errors(self) -> bool:
        return len(self._errors) > 0

    @property
    def errors(self) -> List[SemanticError]:
        return self._errors

    def clear(self):
        self._errors = []
        self._context_stack = []

    @contextmanager
    def context(self, description: str):
        """
        Менеджер контекста для уточнения места ошибки.
        Пример: with collector.context("Inside function 'foo'"):
        """
        self._context_stack.append(description)
        try:
            yield
        finally:
            self._context_stack.pop()