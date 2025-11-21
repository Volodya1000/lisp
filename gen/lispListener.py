# Generated from D:/Volodya/university/yapis/lisp/grammar/lisp.g4 by ANTLR 4.13.2
from antlr4 import *
if "." in __name__:
    from .lispParser import lispParser
else:
    from lispParser import lispParser

# This class defines a complete listener for a parse tree produced by lispParser.
class lispListener(ParseTreeListener):

    # Enter a parse tree produced by lispParser#program.
    def enterProgram(self, ctx:lispParser.ProgramContext):
        pass

    # Exit a parse tree produced by lispParser#program.
    def exitProgram(self, ctx:lispParser.ProgramContext):
        pass


    # Enter a parse tree produced by lispParser#form.
    def enterForm(self, ctx:lispParser.FormContext):
        pass

    # Exit a parse tree produced by lispParser#form.
    def exitForm(self, ctx:lispParser.FormContext):
        pass


    # Enter a parse tree produced by lispParser#sexpr.
    def enterSexpr(self, ctx:lispParser.SexprContext):
        pass

    # Exit a parse tree produced by lispParser#sexpr.
    def exitSexpr(self, ctx:lispParser.SexprContext):
        pass


    # Enter a parse tree produced by lispParser#atom.
    def enterAtom(self, ctx:lispParser.AtomContext):
        pass

    # Exit a parse tree produced by lispParser#atom.
    def exitAtom(self, ctx:lispParser.AtomContext):
        pass


    # Enter a parse tree produced by lispParser#list.
    def enterList(self, ctx:lispParser.ListContext):
        pass

    # Exit a parse tree produced by lispParser#list.
    def exitList(self, ctx:lispParser.ListContext):
        pass



del lispParser