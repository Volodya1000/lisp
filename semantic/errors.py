from typing import Optional


class SemanticError(Exception):
    def __init__(self, message: str, line: Optional[int] = None, column: Optional[int] = None):
        super().__init__(message)
        self.message = message
        self.line = line
        self.column = column

    def __str__(self):
        if self.line is not None:
            return f"[Line {self.line}:{self.column}] {self.message}"
        return self.message


class NameErrorSemantic(SemanticError):
    pass


class DuplicateDeclarationError(SemanticError):
    pass


class AttemptToCallNonFunctionError(SemanticError):
    pass


class ArityError(SemanticError):
    pass


class InvalidBindingError(SemanticError):
    pass


class DivisionByZeroError(SemanticError):
    pass


class InvalidQuoteError(SemanticError):
    pass
