from typing import List
from gen.lispParser import lispParser
from .ast_nodes import *


class QuoteBuilder:

    @classmethod
    def build(cls, ctx: lispParser.SexprContext) -> ASTNode:
        if ctx.atom():
            return cls._build_atom(ctx.atom())
        else:
            return cls._build_list(ctx.list_())

    @classmethod
    def _build_atom(cls, ctx: lispParser.AtomContext) -> ASTNode:
        if ctx.NUMBER(): return NumberNode(float(ctx.NUMBER().getText()))
        if ctx.STRING(): return StringNode(ctx.STRING().getText()[1:-1])
        if ctx.NIL(): return NilNode()
        if ctx.TRUE(): return TrueNode()
        if ctx.SYMBOL():
            name = ctx.SYMBOL().getText()
            if name == 'nil': return NilNode()
            if name == 't': return TrueNode()
            try:
                return NumberNode(float(name))
            except ValueError:
                return SymbolNode(name)
        if ctx.QUOTE():
            return QuoteNode(cls.build(ctx.sexpr()))

        return SymbolNode(ctx.getText())

    @classmethod
    def _build_list(cls, ctx: lispParser.ListContext) -> ListNode:
        elements = [cls.build(x) for x in ctx.sexpr()]
        return ListNode(elements)