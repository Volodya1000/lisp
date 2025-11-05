# Generated from D:/Volodya/university/yapis/lab3/lisp_grammer.g4 by ANTLR 4.13.2
# encoding: utf-8
from antlr4 import *
from io import StringIO
import sys
if sys.version_info[1] > 5:
	from typing import TextIO
else:
	from typing.io import TextIO

def serializedATN():
    return [
        4,1,13,93,2,0,7,0,2,1,7,1,2,2,7,2,2,3,7,3,2,4,7,4,2,5,7,5,2,6,7,
        6,2,7,7,7,2,8,7,8,1,0,5,0,20,8,0,10,0,12,0,23,9,0,1,0,1,0,1,1,1,
        1,1,1,1,1,3,1,31,8,1,1,2,1,2,1,2,1,2,1,2,1,2,4,2,39,8,2,11,2,12,
        2,40,1,2,1,2,1,3,5,3,46,8,3,10,3,12,3,49,9,3,1,4,1,4,1,4,1,4,4,4,
        55,8,4,11,4,12,4,56,1,4,1,4,4,4,61,8,4,11,4,12,4,62,1,4,1,4,1,5,
        1,5,1,5,1,5,1,5,1,6,1,6,5,6,74,8,6,10,6,12,6,77,9,6,1,6,1,6,1,7,
        1,7,1,7,1,7,1,7,1,7,1,7,3,7,88,8,7,1,8,1,8,1,8,1,8,0,0,9,0,2,4,6,
        8,10,12,14,16,0,0,98,0,21,1,0,0,0,2,30,1,0,0,0,4,32,1,0,0,0,6,47,
        1,0,0,0,8,50,1,0,0,0,10,66,1,0,0,0,12,71,1,0,0,0,14,87,1,0,0,0,16,
        89,1,0,0,0,18,20,3,2,1,0,19,18,1,0,0,0,20,23,1,0,0,0,21,19,1,0,0,
        0,21,22,1,0,0,0,22,24,1,0,0,0,23,21,1,0,0,0,24,25,5,0,0,1,25,1,1,
        0,0,0,26,31,3,14,7,0,27,31,3,4,2,0,28,31,3,8,4,0,29,31,3,12,6,0,
        30,26,1,0,0,0,30,27,1,0,0,0,30,28,1,0,0,0,30,29,1,0,0,0,31,3,1,0,
        0,0,32,33,5,1,0,0,33,34,5,2,0,0,34,35,5,1,0,0,35,36,3,6,3,0,36,38,
        5,3,0,0,37,39,3,2,1,0,38,37,1,0,0,0,39,40,1,0,0,0,40,38,1,0,0,0,
        40,41,1,0,0,0,41,42,1,0,0,0,42,43,5,3,0,0,43,5,1,0,0,0,44,46,5,11,
        0,0,45,44,1,0,0,0,46,49,1,0,0,0,47,45,1,0,0,0,47,48,1,0,0,0,48,7,
        1,0,0,0,49,47,1,0,0,0,50,51,5,1,0,0,51,52,5,4,0,0,52,54,5,1,0,0,
        53,55,3,10,5,0,54,53,1,0,0,0,55,56,1,0,0,0,56,54,1,0,0,0,56,57,1,
        0,0,0,57,58,1,0,0,0,58,60,5,3,0,0,59,61,3,2,1,0,60,59,1,0,0,0,61,
        62,1,0,0,0,62,60,1,0,0,0,62,63,1,0,0,0,63,64,1,0,0,0,64,65,5,3,0,
        0,65,9,1,0,0,0,66,67,5,1,0,0,67,68,5,11,0,0,68,69,3,2,1,0,69,70,
        5,3,0,0,70,11,1,0,0,0,71,75,5,1,0,0,72,74,3,2,1,0,73,72,1,0,0,0,
        74,77,1,0,0,0,75,73,1,0,0,0,75,76,1,0,0,0,76,78,1,0,0,0,77,75,1,
        0,0,0,78,79,5,3,0,0,79,13,1,0,0,0,80,88,5,7,0,0,81,88,5,8,0,0,82,
        88,5,9,0,0,83,88,5,11,0,0,84,88,5,5,0,0,85,88,5,6,0,0,86,88,3,16,
        8,0,87,80,1,0,0,0,87,81,1,0,0,0,87,82,1,0,0,0,87,83,1,0,0,0,87,84,
        1,0,0,0,87,85,1,0,0,0,87,86,1,0,0,0,88,15,1,0,0,0,89,90,5,10,0,0,
        90,91,3,2,1,0,91,17,1,0,0,0,8,21,30,40,47,56,62,75,87
    ]

class lisp_grammerParser ( Parser ):

    grammarFileName = "lisp_grammer.g4"

    atn = ATNDeserializer().deserialize(serializedATN())

    decisionsToDFA = [ DFA(ds, i) for i, ds in enumerate(atn.decisionToState) ]

    sharedContextCache = PredictionContextCache()

    literalNames = [ "<INVALID>", "'('", "'lambda'", "')'", "'let'", "'T'", 
                     "'NIL'", "<INVALID>", "<INVALID>", "<INVALID>", "'''" ]

    symbolicNames = [ "<INVALID>", "<INVALID>", "<INVALID>", "<INVALID>", 
                      "<INVALID>", "<INVALID>", "<INVALID>", "INT", "FLOAT", 
                      "STRING", "QUOTE", "SYMBOL", "WS", "COMMENT" ]

    RULE_program = 0
    RULE_expr = 1
    RULE_lambdaExpr = 2
    RULE_paramList = 3
    RULE_letExpr = 4
    RULE_binding = 5
    RULE_list = 6
    RULE_atom = 7
    RULE_quoteExpr = 8

    ruleNames =  [ "program", "expr", "lambdaExpr", "paramList", "letExpr", 
                   "binding", "list", "atom", "quoteExpr" ]

    EOF = Token.EOF
    T__0=1
    T__1=2
    T__2=3
    T__3=4
    T__4=5
    T__5=6
    INT=7
    FLOAT=8
    STRING=9
    QUOTE=10
    SYMBOL=11
    WS=12
    COMMENT=13

    def __init__(self, input:TokenStream, output:TextIO = sys.stdout):
        super().__init__(input, output)
        self.checkVersion("4.13.2")
        self._interp = ParserATNSimulator(self, self.atn, self.decisionsToDFA, self.sharedContextCache)
        self._predicates = None




    class ProgramContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def EOF(self):
            return self.getToken(lisp_grammerParser.EOF, 0)

        def expr(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(lisp_grammerParser.ExprContext)
            else:
                return self.getTypedRuleContext(lisp_grammerParser.ExprContext,i)


        def getRuleIndex(self):
            return lisp_grammerParser.RULE_program

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterProgram" ):
                listener.enterProgram(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitProgram" ):
                listener.exitProgram(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitProgram" ):
                return visitor.visitProgram(self)
            else:
                return visitor.visitChildren(self)




    def program(self):

        localctx = lisp_grammerParser.ProgramContext(self, self._ctx, self.state)
        self.enterRule(localctx, 0, self.RULE_program)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 21
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while (((_la) & ~0x3f) == 0 and ((1 << _la) & 4066) != 0):
                self.state = 18
                self.expr()
                self.state = 23
                self._errHandler.sync(self)
                _la = self._input.LA(1)

            self.state = 24
            self.match(lisp_grammerParser.EOF)
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class ExprContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def atom(self):
            return self.getTypedRuleContext(lisp_grammerParser.AtomContext,0)


        def lambdaExpr(self):
            return self.getTypedRuleContext(lisp_grammerParser.LambdaExprContext,0)


        def letExpr(self):
            return self.getTypedRuleContext(lisp_grammerParser.LetExprContext,0)


        def list_(self):
            return self.getTypedRuleContext(lisp_grammerParser.ListContext,0)


        def getRuleIndex(self):
            return lisp_grammerParser.RULE_expr

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterExpr" ):
                listener.enterExpr(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitExpr" ):
                listener.exitExpr(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitExpr" ):
                return visitor.visitExpr(self)
            else:
                return visitor.visitChildren(self)




    def expr(self):

        localctx = lisp_grammerParser.ExprContext(self, self._ctx, self.state)
        self.enterRule(localctx, 2, self.RULE_expr)
        try:
            self.state = 30
            self._errHandler.sync(self)
            la_ = self._interp.adaptivePredict(self._input,1,self._ctx)
            if la_ == 1:
                self.enterOuterAlt(localctx, 1)
                self.state = 26
                self.atom()
                pass

            elif la_ == 2:
                self.enterOuterAlt(localctx, 2)
                self.state = 27
                self.lambdaExpr()
                pass

            elif la_ == 3:
                self.enterOuterAlt(localctx, 3)
                self.state = 28
                self.letExpr()
                pass

            elif la_ == 4:
                self.enterOuterAlt(localctx, 4)
                self.state = 29
                self.list_()
                pass


        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class LambdaExprContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def paramList(self):
            return self.getTypedRuleContext(lisp_grammerParser.ParamListContext,0)


        def expr(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(lisp_grammerParser.ExprContext)
            else:
                return self.getTypedRuleContext(lisp_grammerParser.ExprContext,i)


        def getRuleIndex(self):
            return lisp_grammerParser.RULE_lambdaExpr

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterLambdaExpr" ):
                listener.enterLambdaExpr(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitLambdaExpr" ):
                listener.exitLambdaExpr(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitLambdaExpr" ):
                return visitor.visitLambdaExpr(self)
            else:
                return visitor.visitChildren(self)




    def lambdaExpr(self):

        localctx = lisp_grammerParser.LambdaExprContext(self, self._ctx, self.state)
        self.enterRule(localctx, 4, self.RULE_lambdaExpr)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 32
            self.match(lisp_grammerParser.T__0)
            self.state = 33
            self.match(lisp_grammerParser.T__1)
            self.state = 34
            self.match(lisp_grammerParser.T__0)
            self.state = 35
            self.paramList()
            self.state = 36
            self.match(lisp_grammerParser.T__2)
            self.state = 38 
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while True:
                self.state = 37
                self.expr()
                self.state = 40 
                self._errHandler.sync(self)
                _la = self._input.LA(1)
                if not ((((_la) & ~0x3f) == 0 and ((1 << _la) & 4066) != 0)):
                    break

            self.state = 42
            self.match(lisp_grammerParser.T__2)
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class ParamListContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def SYMBOL(self, i:int=None):
            if i is None:
                return self.getTokens(lisp_grammerParser.SYMBOL)
            else:
                return self.getToken(lisp_grammerParser.SYMBOL, i)

        def getRuleIndex(self):
            return lisp_grammerParser.RULE_paramList

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterParamList" ):
                listener.enterParamList(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitParamList" ):
                listener.exitParamList(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitParamList" ):
                return visitor.visitParamList(self)
            else:
                return visitor.visitChildren(self)




    def paramList(self):

        localctx = lisp_grammerParser.ParamListContext(self, self._ctx, self.state)
        self.enterRule(localctx, 6, self.RULE_paramList)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 47
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while _la==11:
                self.state = 44
                self.match(lisp_grammerParser.SYMBOL)
                self.state = 49
                self._errHandler.sync(self)
                _la = self._input.LA(1)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class LetExprContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def binding(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(lisp_grammerParser.BindingContext)
            else:
                return self.getTypedRuleContext(lisp_grammerParser.BindingContext,i)


        def expr(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(lisp_grammerParser.ExprContext)
            else:
                return self.getTypedRuleContext(lisp_grammerParser.ExprContext,i)


        def getRuleIndex(self):
            return lisp_grammerParser.RULE_letExpr

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterLetExpr" ):
                listener.enterLetExpr(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitLetExpr" ):
                listener.exitLetExpr(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitLetExpr" ):
                return visitor.visitLetExpr(self)
            else:
                return visitor.visitChildren(self)




    def letExpr(self):

        localctx = lisp_grammerParser.LetExprContext(self, self._ctx, self.state)
        self.enterRule(localctx, 8, self.RULE_letExpr)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 50
            self.match(lisp_grammerParser.T__0)
            self.state = 51
            self.match(lisp_grammerParser.T__3)
            self.state = 52
            self.match(lisp_grammerParser.T__0)
            self.state = 54 
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while True:
                self.state = 53
                self.binding()
                self.state = 56 
                self._errHandler.sync(self)
                _la = self._input.LA(1)
                if not (_la==1):
                    break

            self.state = 58
            self.match(lisp_grammerParser.T__2)
            self.state = 60 
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while True:
                self.state = 59
                self.expr()
                self.state = 62 
                self._errHandler.sync(self)
                _la = self._input.LA(1)
                if not ((((_la) & ~0x3f) == 0 and ((1 << _la) & 4066) != 0)):
                    break

            self.state = 64
            self.match(lisp_grammerParser.T__2)
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class BindingContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def SYMBOL(self):
            return self.getToken(lisp_grammerParser.SYMBOL, 0)

        def expr(self):
            return self.getTypedRuleContext(lisp_grammerParser.ExprContext,0)


        def getRuleIndex(self):
            return lisp_grammerParser.RULE_binding

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterBinding" ):
                listener.enterBinding(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitBinding" ):
                listener.exitBinding(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitBinding" ):
                return visitor.visitBinding(self)
            else:
                return visitor.visitChildren(self)




    def binding(self):

        localctx = lisp_grammerParser.BindingContext(self, self._ctx, self.state)
        self.enterRule(localctx, 10, self.RULE_binding)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 66
            self.match(lisp_grammerParser.T__0)
            self.state = 67
            self.match(lisp_grammerParser.SYMBOL)
            self.state = 68
            self.expr()
            self.state = 69
            self.match(lisp_grammerParser.T__2)
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class ListContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def expr(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(lisp_grammerParser.ExprContext)
            else:
                return self.getTypedRuleContext(lisp_grammerParser.ExprContext,i)


        def getRuleIndex(self):
            return lisp_grammerParser.RULE_list

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterList" ):
                listener.enterList(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitList" ):
                listener.exitList(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitList" ):
                return visitor.visitList(self)
            else:
                return visitor.visitChildren(self)




    def list_(self):

        localctx = lisp_grammerParser.ListContext(self, self._ctx, self.state)
        self.enterRule(localctx, 12, self.RULE_list)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 71
            self.match(lisp_grammerParser.T__0)
            self.state = 75
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while (((_la) & ~0x3f) == 0 and ((1 << _la) & 4066) != 0):
                self.state = 72
                self.expr()
                self.state = 77
                self._errHandler.sync(self)
                _la = self._input.LA(1)

            self.state = 78
            self.match(lisp_grammerParser.T__2)
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class AtomContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def INT(self):
            return self.getToken(lisp_grammerParser.INT, 0)

        def FLOAT(self):
            return self.getToken(lisp_grammerParser.FLOAT, 0)

        def STRING(self):
            return self.getToken(lisp_grammerParser.STRING, 0)

        def SYMBOL(self):
            return self.getToken(lisp_grammerParser.SYMBOL, 0)

        def quoteExpr(self):
            return self.getTypedRuleContext(lisp_grammerParser.QuoteExprContext,0)


        def getRuleIndex(self):
            return lisp_grammerParser.RULE_atom

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterAtom" ):
                listener.enterAtom(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitAtom" ):
                listener.exitAtom(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitAtom" ):
                return visitor.visitAtom(self)
            else:
                return visitor.visitChildren(self)




    def atom(self):

        localctx = lisp_grammerParser.AtomContext(self, self._ctx, self.state)
        self.enterRule(localctx, 14, self.RULE_atom)
        try:
            self.state = 87
            self._errHandler.sync(self)
            token = self._input.LA(1)
            if token in [7]:
                self.enterOuterAlt(localctx, 1)
                self.state = 80
                self.match(lisp_grammerParser.INT)
                pass
            elif token in [8]:
                self.enterOuterAlt(localctx, 2)
                self.state = 81
                self.match(lisp_grammerParser.FLOAT)
                pass
            elif token in [9]:
                self.enterOuterAlt(localctx, 3)
                self.state = 82
                self.match(lisp_grammerParser.STRING)
                pass
            elif token in [11]:
                self.enterOuterAlt(localctx, 4)
                self.state = 83
                self.match(lisp_grammerParser.SYMBOL)
                pass
            elif token in [5]:
                self.enterOuterAlt(localctx, 5)
                self.state = 84
                self.match(lisp_grammerParser.T__4)
                pass
            elif token in [6]:
                self.enterOuterAlt(localctx, 6)
                self.state = 85
                self.match(lisp_grammerParser.T__5)
                pass
            elif token in [10]:
                self.enterOuterAlt(localctx, 7)
                self.state = 86
                self.quoteExpr()
                pass
            else:
                raise NoViableAltException(self)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class QuoteExprContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def QUOTE(self):
            return self.getToken(lisp_grammerParser.QUOTE, 0)

        def expr(self):
            return self.getTypedRuleContext(lisp_grammerParser.ExprContext,0)


        def getRuleIndex(self):
            return lisp_grammerParser.RULE_quoteExpr

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterQuoteExpr" ):
                listener.enterQuoteExpr(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitQuoteExpr" ):
                listener.exitQuoteExpr(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitQuoteExpr" ):
                return visitor.visitQuoteExpr(self)
            else:
                return visitor.visitChildren(self)




    def quoteExpr(self):

        localctx = lisp_grammerParser.QuoteExprContext(self, self._ctx, self.state)
        self.enterRule(localctx, 16, self.RULE_quoteExpr)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 89
            self.match(lisp_grammerParser.QUOTE)
            self.state = 90
            self.expr()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx





