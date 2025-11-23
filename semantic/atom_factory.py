from gen.lispParser import lispParser
from .ast_nodes import *

class AtomFactory:
    @staticmethod
    def create(ctx: lispParser.AtomContext) -> ASTNode:
        if ctx.NUMBER():
            return NumberNode(float(ctx.NUMBER().getText()))
        elif ctx.STRING():
            # Убираем кавычки
            return StringNode(ctx.STRING().getText()[1:-1])
        elif ctx.SYMBOL():
            name = ctx.SYMBOL().getText()
            # Попытка интерпретировать символ как число
            try:
                return NumberNode(float(name))
            except ValueError:
                pass

            if name == 'nil':
                return NilNode()
            elif name == 't':
                return TrueNode()
            else:
                return SymbolNode(name)
        elif ctx.NIL():
            return NilNode()
        elif ctx.TRUE():
            return TrueNode()
        elif ctx.QUOTE():
            from .quote_builder import QuoteBuilder
            return QuoteNode(QuoteBuilder.build(ctx.sexpr()))
        else:
            # Fallback
            return NilNode()