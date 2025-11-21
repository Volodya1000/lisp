# Generated from D:/Volodya/university/yapis/lisp/grammar/lisp.g4 by ANTLR 4.13.2
from antlr4 import *
if "." in __name__:
    from .lispParser import lispParser
else:
    from lispParser import lispParser

# This class defines a complete generic visitor for a parse tree produced by lispParser.

class lispVisitor(ParseTreeVisitor):

    # Visit a parse tree produced by lispParser#program.
    def visitProgram(self, ctx:lispParser.ProgramContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by lispParser#form.
    def visitForm(self, ctx:lispParser.FormContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by lispParser#sexpr.
    def visitSexpr(self, ctx:lispParser.SexprContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by lispParser#atom.
    def visitAtom(self, ctx:lispParser.AtomContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by lispParser#list.
    def visitList(self, ctx:lispParser.ListContext):
        return self.visitChildren(ctx)



del lispParser