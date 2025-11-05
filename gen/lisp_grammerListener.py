# Generated from D:/Volodya/university/yapis/lab3/lisp_grammer.g4 by ANTLR 4.13.2
from antlr4 import *
if "." in __name__:
    from .lisp_grammerParser import lisp_grammerParser
else:
    from lisp_grammerParser import lisp_grammerParser

# This class defines a complete listener for a parse tree produced by lisp_grammerParser.
class lisp_grammerListener(ParseTreeListener):

    # Enter a parse tree produced by lisp_grammerParser#program.
    def enterProgram(self, ctx:lisp_grammerParser.ProgramContext):
        pass

    # Exit a parse tree produced by lisp_grammerParser#program.
    def exitProgram(self, ctx:lisp_grammerParser.ProgramContext):
        pass


    # Enter a parse tree produced by lisp_grammerParser#expr.
    def enterExpr(self, ctx:lisp_grammerParser.ExprContext):
        pass

    # Exit a parse tree produced by lisp_grammerParser#expr.
    def exitExpr(self, ctx:lisp_grammerParser.ExprContext):
        pass


    # Enter a parse tree produced by lisp_grammerParser#lambdaExpr.
    def enterLambdaExpr(self, ctx:lisp_grammerParser.LambdaExprContext):
        pass

    # Exit a parse tree produced by lisp_grammerParser#lambdaExpr.
    def exitLambdaExpr(self, ctx:lisp_grammerParser.LambdaExprContext):
        pass


    # Enter a parse tree produced by lisp_grammerParser#paramList.
    def enterParamList(self, ctx:lisp_grammerParser.ParamListContext):
        pass

    # Exit a parse tree produced by lisp_grammerParser#paramList.
    def exitParamList(self, ctx:lisp_grammerParser.ParamListContext):
        pass


    # Enter a parse tree produced by lisp_grammerParser#letExpr.
    def enterLetExpr(self, ctx:lisp_grammerParser.LetExprContext):
        pass

    # Exit a parse tree produced by lisp_grammerParser#letExpr.
    def exitLetExpr(self, ctx:lisp_grammerParser.LetExprContext):
        pass


    # Enter a parse tree produced by lisp_grammerParser#binding.
    def enterBinding(self, ctx:lisp_grammerParser.BindingContext):
        pass

    # Exit a parse tree produced by lisp_grammerParser#binding.
    def exitBinding(self, ctx:lisp_grammerParser.BindingContext):
        pass


    # Enter a parse tree produced by lisp_grammerParser#list.
    def enterList(self, ctx:lisp_grammerParser.ListContext):
        pass

    # Exit a parse tree produced by lisp_grammerParser#list.
    def exitList(self, ctx:lisp_grammerParser.ListContext):
        pass


    # Enter a parse tree produced by lisp_grammerParser#atom.
    def enterAtom(self, ctx:lisp_grammerParser.AtomContext):
        pass

    # Exit a parse tree produced by lisp_grammerParser#atom.
    def exitAtom(self, ctx:lisp_grammerParser.AtomContext):
        pass


    # Enter a parse tree produced by lisp_grammerParser#quoteExpr.
    def enterQuoteExpr(self, ctx:lisp_grammerParser.QuoteExprContext):
        pass

    # Exit a parse tree produced by lisp_grammerParser#quoteExpr.
    def exitQuoteExpr(self, ctx:lisp_grammerParser.QuoteExprContext):
        pass



del lisp_grammerParser