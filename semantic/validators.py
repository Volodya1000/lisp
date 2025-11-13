from typing import List, Optional
from semantic.errors import ArityError, SemanticError
from semantic.scope import Scope
from semantic.utils import get_position, extract_integer_literal
from semantic.errors import DivisionByZeroError

class CallValidator:
    """Валидатор вызовов функций и лямбд."""

    def __init__(self, scope: Scope):
        self.scope = scope

    def validate_function_call(self, func_name: str, args: List, ctx) -> Optional[ArityError]:
        """Проверяет арность вызова функции по имени."""
        symbol = self.scope.lookup(func_name)
        if not symbol or symbol.param_count is None:
            return None

        expected = symbol.param_count
        got = len(args)
        if expected != got:
            line, col = get_position(ctx)
            return ArityError(
                f"Function '{func_name}' expects {expected} arguments, got {got}",
                line, col
            )
        return None

    def validate_lambda_call(self, lambda_ctx, args: List, ctx) -> Optional[ArityError]:
        """Проверяет арность вызова inline lambda."""
        param_list_ctx = lambda_ctx.paramList()
        if param_list_ctx is None:
            return None

        param_symbols = getattr(param_list_ctx, "SYMBOL", lambda: [])()
        expected = len(param_symbols)
        got = len(args)

        if expected != got:
            line, col = get_position(ctx)
            return ArityError(
                f"Inline lambda expects {expected} args, got {got}",
                line, col
            )
        return None

    def validate_format_call(self, args: List, ctx) -> Optional[SemanticError]:
        """Проверяет, что format имеет хотя бы 2 аргумента (строка + значение)."""
        if len(args) < 2:
            line, col = get_position(ctx)
            return ArityError("format requires at least 2 arguments (format-string & value)", line, col)
        return None


class DivisionByZeroValidator:
    """Статический валидатор деления на ноль."""

    @staticmethod
    def validate_division(args: List, position_func) -> Optional[DivisionByZeroError]:
        """Проверяет деление на ноль для операции '/'."""
        from semantic.errors import DivisionByZeroError

        if len(args) < 2:
            return None

        divisor = extract_integer_literal(args[1])
        if divisor == 0:
            line, col = position_func(args[1])
            return DivisionByZeroError("Division by zero detected (statically)", line, col)
        return None