from typing import Optional


def _pos_from_ctx(ctx) -> (Optional[int], Optional[int]):
    if ctx is None:
        return None, None
    try:
        # Пытаемся получить 'start' токен, который есть у правил
        token = getattr(ctx, "start", None)
        if token is not None:
            # ANTLR токен имеет поле 'line' и 'column'
            return getattr(token, "line", None), getattr(token, "column", None)

        # Если 'start' нет (например, это сам токен)
        if hasattr(ctx, "line"):
            return getattr(ctx, "line", None), getattr(ctx, "column", None)

        # Для TerminalNode
        if hasattr(ctx, "symbol"):
            return getattr(ctx.symbol, "line", None), getattr(ctx.symbol, "column", None)

    except Exception:
        pass
    return None, None


def _is_int_literal(expr_ctx) -> Optional[int]:
    try:
        # atom -> INT | FLOAT | STRING | SYMBOL | 'T' | 'NIL' | quoteExpr
        if hasattr(expr_ctx, "atom") and expr_ctx.atom() is not None:
            atom = expr_ctx.atom()
            if getattr(atom, "INT", None) is not None and atom.INT() is not None:
                text = atom.INT().getText()
                try:
                    return int(text)
                except Exception:
                    return None
    except Exception:
        pass
    return None
