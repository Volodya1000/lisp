# Generated from D:/Volodya/university/yapis/lisp/grammar/lisp.g4 by ANTLR 4.13.2
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
        4,1,10,43,2,0,7,0,2,1,7,1,2,2,7,2,2,3,7,3,2,4,7,4,1,0,5,0,12,8,0,
        10,0,12,0,15,9,0,1,0,1,0,1,1,1,1,1,2,1,2,3,2,23,8,2,1,3,1,3,1,3,
        1,3,1,3,1,3,1,3,3,3,32,8,3,1,4,1,4,5,4,36,8,4,10,4,12,4,39,9,4,1,
        4,1,4,1,4,0,0,5,0,2,4,6,8,0,0,45,0,13,1,0,0,0,2,18,1,0,0,0,4,22,
        1,0,0,0,6,31,1,0,0,0,8,33,1,0,0,0,10,12,3,2,1,0,11,10,1,0,0,0,12,
        15,1,0,0,0,13,11,1,0,0,0,13,14,1,0,0,0,14,16,1,0,0,0,15,13,1,0,0,
        0,16,17,5,0,0,1,17,1,1,0,0,0,18,19,3,4,2,0,19,3,1,0,0,0,20,23,3,
        6,3,0,21,23,3,8,4,0,22,20,1,0,0,0,22,21,1,0,0,0,23,5,1,0,0,0,24,
        32,5,4,0,0,25,32,5,5,0,0,26,32,5,3,0,0,27,28,5,6,0,0,28,32,3,4,2,
        0,29,32,5,7,0,0,30,32,5,8,0,0,31,24,1,0,0,0,31,25,1,0,0,0,31,26,
        1,0,0,0,31,27,1,0,0,0,31,29,1,0,0,0,31,30,1,0,0,0,32,7,1,0,0,0,33,
        37,5,1,0,0,34,36,3,4,2,0,35,34,1,0,0,0,36,39,1,0,0,0,37,35,1,0,0,
        0,37,38,1,0,0,0,38,40,1,0,0,0,39,37,1,0,0,0,40,41,5,2,0,0,41,9,1,
        0,0,0,4,13,22,31,37
    ]

class lispParser ( Parser ):

    grammarFileName = "lisp.g4"

    atn = ATNDeserializer().deserialize(serializedATN())

    decisionsToDFA = [ DFA(ds, i) for i, ds in enumerate(atn.decisionToState) ]

    sharedContextCache = PredictionContextCache()

    literalNames = [ "<INVALID>", "'('", "')'", "<INVALID>", "<INVALID>", 
                     "<INVALID>", "'''", "'nil'", "'t'" ]

    symbolicNames = [ "<INVALID>", "<INVALID>", "<INVALID>", "SYMBOL", "NUMBER", 
                      "STRING", "QUOTE", "NIL", "TRUE", "COMMENT", "WS" ]

    RULE_program = 0
    RULE_form = 1
    RULE_sexpr = 2
    RULE_atom = 3
    RULE_list = 4

    ruleNames =  [ "program", "form", "sexpr", "atom", "list" ]

    EOF = Token.EOF
    T__0=1
    T__1=2
    SYMBOL=3
    NUMBER=4
    STRING=5
    QUOTE=6
    NIL=7
    TRUE=8
    COMMENT=9
    WS=10

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
            return self.getToken(lispParser.EOF, 0)

        def form(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(lispParser.FormContext)
            else:
                return self.getTypedRuleContext(lispParser.FormContext,i)


        def getRuleIndex(self):
            return lispParser.RULE_program

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

        localctx = lispParser.ProgramContext(self, self._ctx, self.state)
        self.enterRule(localctx, 0, self.RULE_program)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 13
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while (((_la) & ~0x3f) == 0 and ((1 << _la) & 506) != 0):
                self.state = 10
                self.form()
                self.state = 15
                self._errHandler.sync(self)
                _la = self._input.LA(1)

            self.state = 16
            self.match(lispParser.EOF)
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class FormContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def sexpr(self):
            return self.getTypedRuleContext(lispParser.SexprContext,0)


        def getRuleIndex(self):
            return lispParser.RULE_form

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterForm" ):
                listener.enterForm(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitForm" ):
                listener.exitForm(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitForm" ):
                return visitor.visitForm(self)
            else:
                return visitor.visitChildren(self)




    def form(self):

        localctx = lispParser.FormContext(self, self._ctx, self.state)
        self.enterRule(localctx, 2, self.RULE_form)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 18
            self.sexpr()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class SexprContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def atom(self):
            return self.getTypedRuleContext(lispParser.AtomContext,0)


        def list_(self):
            return self.getTypedRuleContext(lispParser.ListContext,0)


        def getRuleIndex(self):
            return lispParser.RULE_sexpr

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterSexpr" ):
                listener.enterSexpr(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitSexpr" ):
                listener.exitSexpr(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitSexpr" ):
                return visitor.visitSexpr(self)
            else:
                return visitor.visitChildren(self)




    def sexpr(self):

        localctx = lispParser.SexprContext(self, self._ctx, self.state)
        self.enterRule(localctx, 4, self.RULE_sexpr)
        try:
            self.state = 22
            self._errHandler.sync(self)
            token = self._input.LA(1)
            if token in [3, 4, 5, 6, 7, 8]:
                self.enterOuterAlt(localctx, 1)
                self.state = 20
                self.atom()
                pass
            elif token in [1]:
                self.enterOuterAlt(localctx, 2)
                self.state = 21
                self.list_()
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


    class AtomContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def NUMBER(self):
            return self.getToken(lispParser.NUMBER, 0)

        def STRING(self):
            return self.getToken(lispParser.STRING, 0)

        def SYMBOL(self):
            return self.getToken(lispParser.SYMBOL, 0)

        def QUOTE(self):
            return self.getToken(lispParser.QUOTE, 0)

        def sexpr(self):
            return self.getTypedRuleContext(lispParser.SexprContext,0)


        def NIL(self):
            return self.getToken(lispParser.NIL, 0)

        def TRUE(self):
            return self.getToken(lispParser.TRUE, 0)

        def getRuleIndex(self):
            return lispParser.RULE_atom

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

        localctx = lispParser.AtomContext(self, self._ctx, self.state)
        self.enterRule(localctx, 6, self.RULE_atom)
        try:
            self.state = 31
            self._errHandler.sync(self)
            token = self._input.LA(1)
            if token in [4]:
                self.enterOuterAlt(localctx, 1)
                self.state = 24
                self.match(lispParser.NUMBER)
                pass
            elif token in [5]:
                self.enterOuterAlt(localctx, 2)
                self.state = 25
                self.match(lispParser.STRING)
                pass
            elif token in [3]:
                self.enterOuterAlt(localctx, 3)
                self.state = 26
                self.match(lispParser.SYMBOL)
                pass
            elif token in [6]:
                self.enterOuterAlt(localctx, 4)
                self.state = 27
                self.match(lispParser.QUOTE)
                self.state = 28
                self.sexpr()
                pass
            elif token in [7]:
                self.enterOuterAlt(localctx, 5)
                self.state = 29
                self.match(lispParser.NIL)
                pass
            elif token in [8]:
                self.enterOuterAlt(localctx, 6)
                self.state = 30
                self.match(lispParser.TRUE)
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


    class ListContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def sexpr(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(lispParser.SexprContext)
            else:
                return self.getTypedRuleContext(lispParser.SexprContext,i)


        def getRuleIndex(self):
            return lispParser.RULE_list

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

        localctx = lispParser.ListContext(self, self._ctx, self.state)
        self.enterRule(localctx, 8, self.RULE_list)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 33
            self.match(lispParser.T__0)
            self.state = 37
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while (((_la) & ~0x3f) == 0 and ((1 << _la) & 506) != 0):
                self.state = 34
                self.sexpr()
                self.state = 39
                self._errHandler.sync(self)
                _la = self._input.LA(1)

            self.state = 40
            self.match(lispParser.T__1)
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx





