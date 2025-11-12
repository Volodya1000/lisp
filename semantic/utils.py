from typing import Optional, Tuple
from semantic.errors import SemanticError


def get_position(ctx) -> Tuple[Optional[int], Optional[int]]:
    """Извлекает позицию (line, column) из ANTLR контекста."""
    if ctx is None:
        return None, None

    # Для правил (RuleContext)
    if hasattr(ctx, "start") and ctx.start is not None:
        return getattr(ctx.start, "line", None), getattr(ctx.start, "column", None)

    # Для терминалов (TerminalNode)
    if hasattr(ctx, "symbol") and ctx.symbol is not None:
        return getattr(ctx.symbol, "line", None), getattr(ctx.symbol, "column", None)

    # Для самих токенов
    if hasattr(ctx, "line"):
        return getattr(ctx, "line", None), getattr(ctx, "column", None)

    return None, None


def extract_integer_literal(expr_ctx) -> Optional[int]:
    """Пытается извлечь целое число из выражения (для static анализа)."""
    try:
        if hasattr(expr_ctx, "atom") and expr_ctx.atom() is not None:
            atom = expr_ctx.atom()
            if hasattr(atom, "INT") and atom.INT() is not None:
                text = atom.INT().getText()
                return int(text)
    except (AttributeError, ValueError):
        pass
    return None


def extract_symbol_name(ctx) -> Optional[str]:
    """Извлекает имя символа из expr или atom контекста."""
    try:
        # Если это expr с атомом
        if hasattr(ctx, "atom") and ctx.atom() is not None:
            atom = ctx.atom()
            if hasattr(atom, "SYMBOL") and atom.SYMBOL() is not None:
                return atom.SYMBOL().getText()
        # Если это сам atom
        elif hasattr(ctx, "SYMBOL") and ctx.SYMBOL() is not None:
            return ctx.SYMBOL().getText()
    except AttributeError:
        pass
    return None


def is_lambda_expression(ctx) -> bool:
    """Проверяет, является ли контекст lambda-выражением."""
    return hasattr(ctx, "lambdaExpr") and ctx.lambdaExpr() is not None


def get_child_expressions(ctx) -> list:
    """Безопасно получает список дочерних expr контекстов."""
    return getattr(ctx, "expr", lambda *a: [])() or []


def is_literal_atom(ctx) -> bool:
    """Проверяет, является ли expr контекст литеральным атомом."""
    try:
        if hasattr(ctx, "atom") and ctx.atom() is not None:
            atom = ctx.atom()
            # Не должно быть символа
            if hasattr(atom, "SYMBOL") and atom.SYMBOL() is not None:
                return False
            # Не должно быть quote
            if hasattr(atom, "quoteExpr") and atom.quoteExpr() is not None:
                return False
            # Не должно быть lambda на уровне expr
            if hasattr(ctx, "lambdaExpr") and ctx.lambdaExpr() is not None:
                return False
            # Если есть хоть один литеральный токен - это литерал
            literal_attrs = ["INT", "FLOAT", "STRING", "T", "NIL"]
            for attr in literal_attrs:
                if hasattr(atom, attr) and getattr(atom, attr)() is not None:
                    return True
    except AttributeError:
        pass
    return False


def extract_lambda_context(expr_ctx) -> Optional[object]:
    """Извлекает LambdaExprContext из ExprContext."""
    if is_lambda_expression(expr_ctx):
        return expr_ctx.lambdaExpr()
    return None