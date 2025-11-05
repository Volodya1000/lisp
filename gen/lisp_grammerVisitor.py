# Generated from D:/Volodya/university/yapis/lab3/lisp_grammer.g4 by ANTLR 4.13.2
from antlr4 import *
if "." in __name__:
    from .lisp_grammerParser import lisp_grammerParser
else:
    from lisp_grammerParser import lisp_grammerParser

# This class defines a complete generic visitor for a parse tree produced by lisp_grammerParser.

class lisp_grammerVisitor(ParseTreeVisitor):

    # Visit a parse tree produced by lisp_grammerParser#program.
    def visitProgram(self, ctx:lisp_grammerParser.ProgramContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by lisp_grammerParser#expr.
    def visitExpr(self, ctx:lisp_grammerParser.ExprContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by lisp_grammerParser#lambdaExpr.
    def visitLambdaExpr(self, ctx:lisp_grammerParser.LambdaExprContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by lisp_grammerParser#paramList.
    def visitParamList(self, ctx:lisp_grammerParser.ParamListContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by lisp_grammerParser#letExpr.
    def visitLetExpr(self, ctx:lisp_grammerParser.LetExprContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by lisp_grammerParser#binding.
    def visitBinding(self, ctx:lisp_grammerParser.BindingContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by lisp_grammerParser#list.
    def visitList(self, ctx:lisp_grammerParser.ListContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by lisp_grammerParser#atom.
    def visitAtom(self, ctx:lisp_grammerParser.AtomContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by lisp_grammerParser#quoteExpr.
    def visitQuoteExpr(self, ctx:lisp_grammerParser.QuoteExprContext):
        return self.visitChildren(ctx)



del lisp_grammerParser