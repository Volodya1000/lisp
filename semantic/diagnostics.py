from dataclasses import dataclass, field
from enum import Enum, auto
from typing import List, Optional, Any


class Severity(Enum):
    ERROR = auto()
    WARNING = auto()

@dataclass(frozen=True, kw_only=True)
class SourceSpan:
    """
    Точная локация в коде.
    """
    start_line: int
    start_col: int
    end_line: int
    end_col: int
    start_index: int = -1
    stop_index: int = -1


@dataclass(kw_only=True)
class SemanticError:
    """Базовый класс семантической ошибки."""
    message: str
    span: SourceSpan
    severity: Severity = Severity.ERROR
    # Стек контекста
    context_stack: List[str] = field(default_factory=list)

    @property
    def error_code(self) -> str:
        return "SEM000"




@dataclass(kw_only=True)
class ArityError(SemanticError):
    func_name: str
    expected_count: Any  # int или строка диапазона
    actual_count: int

    @property
    def error_code(self) -> str:
        return "SEM001"


@dataclass(kw_only=True)
class TypeMismatchError(SemanticError):
    expected_type: str
    actual_type: str

    @property
    def error_code(self) -> str:
        return "SEM002"


@dataclass(kw_only=True)
class InvalidSyntaxError(SemanticError):
    """Для ошибок структуры спецформ"""
    expected_syntax: str

    @property
    def error_code(self) -> str:
        return "SEM003"


@dataclass(kw_only=True)
class UndefinedSymbolError(SemanticError):
    symbol_name: str

    @property
    def error_code(self) -> str:
        return "SEM004"