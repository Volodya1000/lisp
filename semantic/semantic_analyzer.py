from typing import List, Dict
from gen.lispParser import lispParser
from gen.lispVisitor import lispVisitor
from .ast_nodes import *
from .symbol_table import Environment
from .diagnostics import SourceSpan
from .error_collector import ErrorCollector
from .atom_factory import AtomFactory
from .handlers import (
    SpecialFormHandler, QuoteHandler, LambdaHandler, DefunHandler,
    SetqHandler, CondHandler, PrognHandler, LogicHandler, LetHandler
)


class SemanticAnalyzer(lispVisitor):
    """
    Проходит по Parse Tree, строит AST и собирает семантические ошибки.
    Делегирует обработку специальных форм и атомов специализированным классам.
    """

    def __init__(self):
        self.collector = ErrorCollector()
        self.global_env = Environment()
        self.current_env = self.global_env

        self._init_primitives()
        self._init_handlers()

    def _init_primitives(self):
        for name in Environment.PRIMITIVES:
            self.global_env.define(name, is_function=True)
        for name in Environment.SPECIAL_FORMS:
            self.global_env.define(name, is_function=False)

    def _init_handlers(self):
        """Регистрация обработчиков специальных форм."""
        self.handlers: Dict[str, SpecialFormHandler] = {
            'quote': QuoteHandler(),
            'setq': SetqHandler(),
            'lambda': LambdaHandler(),
            'defun': DefunHandler(),
            'cond': CondHandler(),
            'progn': PrognHandler(),
            'and': LogicHandler('and'),
            'or': LogicHandler('or'),
            'let': LetHandler()
        }

    def get_span(self, ctx) -> SourceSpan:
        """Public helper для извлечения координат (используется хендлерами)."""
        if hasattr(ctx, 'start') and ctx.start:
            start = ctx.start
            stop = ctx.stop if ctx.stop else start
            return SourceSpan(
                start_line=start.line,
                start_col=start.column,
                end_line=stop.line,
                end_col=stop.column + len(stop.text),
                start_index=start.start,
                stop_index=stop.stop
            )
        return SourceSpan(0, 0, 0, 0)

    # --- Visits ---

    def visitProgram(self, ctx: lispParser.ProgramContext) -> List[ASTNode]:
        return [self.visit(form) for form in ctx.form()]

    def visitForm(self, ctx: lispParser.FormContext) -> ASTNode:
        return self.visit(ctx.sexpr())

    def visitSexpr(self, ctx: lispParser.SexprContext) -> ASTNode:
        if ctx.atom():
            return self.visit(ctx.atom())
        else:
            return self.visit(ctx.list_())

    def visitAtom(self, ctx: lispParser.AtomContext) -> ASTNode:
        return AtomFactory.create(ctx)

    def visitList(self, ctx: lispParser.ListContext) -> ASTNode:
        sexprs = ctx.sexpr()
        if not sexprs:
            return NilNode()

        # Анализируем первый элемент (голова списка)
        first_ctx = sexprs[0]
        first_node = self.visit(first_ctx)

        if isinstance(first_node, NilNode):
            rest_nodes = [self.visit(e) for e in sexprs[1:]]
            return PrimCallNode('nil', rest_nodes)

        # 1. Special Forms (Dispatch to Handlers)
        if isinstance(first_node, SymbolNode) and first_node.name in self.handlers:
            handler = self.handlers[first_node.name]
            with self.collector.context(f"Special Form '{first_node.name}'"):
                return handler.handle(self, sexprs[1:], ctx)

        # 2. Function Calls (Arguments Evaluation)
        rest_nodes = [self.visit(e) for e in sexprs[1:]]

        # 3. Primitive Calls
        if isinstance(first_node, SymbolNode) and first_node.name in Environment.PRIMITIVES:
            return PrimCallNode(first_node.name, rest_nodes)

        # 4. User Function Calls / Lambda Calls
        return CallNode(first_node, rest_nodes)