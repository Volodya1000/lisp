from typing import List
from gen.lispParser import lispParser
from gen.lispVisitor import lispVisitor
from .ast_nodes import *
from .symbol_table import Environment
from .diagnostics import *
from .error_collector import ErrorCollector


class SemanticAnalyzer(lispVisitor):
    """
    Проходит по Parse Tree, строит AST и собирает семантические ошибки
    в ErrorCollector, не прерывая выполнение (Recovery Mode).
    """

    def __init__(self):
        self.collector = ErrorCollector()
        self.global_env = Environment()
        self.current_env = self.global_env
        self._init_primitives()

    def _init_primitives(self):
        for name in Environment.PRIMITIVES:
            self.global_env.define(name, is_function=True)
        for name in Environment.SPECIAL_FORMS:
            self.global_env.define(name, is_function=False)

    def _get_span(self, ctx) -> SourceSpan:
        """Извлекает координаты из контекста ANTLR."""
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
        if ctx.NUMBER():
            return NumberNode(float(ctx.NUMBER().getText()))
        elif ctx.STRING():
            return StringNode(ctx.STRING().getText()[1:-1])
        elif ctx.SYMBOL():
            name = ctx.SYMBOL().getText()
            # Попытка интерпретировать как число (на всякий случай)
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
            # Обработка 'expr
            return QuoteNode(self._build_quoted_data(ctx.sexpr()))
        else:
            # Recovery: возвращаем NilNode, но вообще это ошибка парсера, сюда редко доходит
            return NilNode()

    def visitList(self, ctx: lispParser.ListContext) -> ASTNode:
        sexprs = ctx.sexpr()
        if not sexprs:
            return NilNode()

        # Анализируем первый элемент (голова списка)
        first_ctx = sexprs[0]
        first_node = self.visit(first_ctx)

        # 1. Special Forms
        if isinstance(first_node, SymbolNode) and first_node.name in Environment.SPECIAL_FORMS:
            with self.collector.context(f"Special Form '{first_node.name}'"):
                return self._handle_special_form_lazy(first_node.name, sexprs[1:], ctx)

        # 2. Function Calls (Arguments Evaluation)
        rest_nodes = [self.visit(e) for e in sexprs[1:]]

        # 3. Primitive Calls
        if isinstance(first_node, SymbolNode) and first_node.name in Environment.PRIMITIVES:
            return PrimCallNode(first_node.name, rest_nodes)

        # 4. User Function Calls / Lambda Calls
        return CallNode(first_node, rest_nodes)

    def _handle_special_form_lazy(self, name: str, raw_args: List[lispParser.SexprContext], parent_ctx) -> ASTNode:
        if name == 'quote':
            return self._handle_quote(raw_args, parent_ctx)
        elif name == 'lambda':
            return self._handle_lambda(raw_args, parent_ctx)
        elif name == 'cond':
            return self._handle_cond(raw_args, parent_ctx)
        elif name == 'setq':
            return self._handle_setq(raw_args, parent_ctx)
        elif name == 'defun':
            return self._handle_defun(raw_args, parent_ctx)
        elif name == 'progn':
            return self._handle_progn(raw_args)
        elif name == 'and' or name == 'or':
            return self._handle_logic(name, raw_args)

        return NilNode()

    # --- Handlers with Error Collection (Recovery Mode) ---

    def _handle_quote(self, raw_args, ctx) -> QuoteNode:
        # ARITY CHECK
        if len(raw_args) != 1:
            self.collector.add_error(ArityError(
                message=f"quote expects 1 argument, got {len(raw_args)}",
                span=self._get_span(ctx),
                func_name="quote",
                expected_count=1,
                actual_count=len(raw_args)
            ))
            # RECOVERY: Если аргументов > 0, берем первый, иначе Nil
            if raw_args:
                return QuoteNode(self._build_quoted_data(raw_args[0]))
            return QuoteNode(NilNode())

        return QuoteNode(self._build_quoted_data(raw_args[0]))

    def _handle_setq(self, raw_args, ctx) -> ASTNode:
        # ARITY CHECK
        if len(raw_args) != 2:
            self.collector.add_error(ArityError(
                message=f"setq expects 2 arguments, got {len(raw_args)}",
                span=self._get_span(ctx),
                func_name="setq",
                expected_count=2,
                actual_count=len(raw_args)
            ))
            # RECOVERY: пытаемся вернуть хоть что-то
            return NilNode()

        # TYPE CHECK (1st arg must be symbol)
        name_node = self.visit(raw_args[0])
        var_name = "error_placeholder"

        if not isinstance(name_node, SymbolNode):
            self.collector.add_error(TypeMismatchError(
                message=f"setq first argument must be a symbol, got {type(name_node).__name__}",
                span=self._get_span(raw_args[0]),
                expected_type="Symbol",
                actual_type=type(name_node).__name__
            ))
        else:
            var_name = name_node.name

        value_node = self.visit(raw_args[1])

        # LOGIC: Define if not exists
        existing = self.current_env.resolve(var_name)
        if not existing:
            self.current_env.define(var_name)

        return SetqNode(var_name, value_node)

    def _handle_lambda(self, raw_args, ctx) -> ASTNode:
        if len(raw_args) < 1:
            self.collector.add_error(ArityError(
                message="lambda requires at least parameters list",
                span=self._get_span(ctx),
                func_name="lambda",
                expected_count=">= 1",
                actual_count=len(raw_args)
            ))
            return NilNode()

        params_node = self.visit(raw_args[0])
        params = self._extract_params(params_node, raw_args[0])

        lambda_env = Environment(parent=self.current_env)
        previous_env = self.current_env
        self.current_env = lambda_env

        body = []
        try:
            for p in params:
                self.current_env.define(p)

            # RECOVERY: Анализируем тело даже если были ошибки в параметрах
            with self.collector.context("Lambda Body"):
                body = [self.visit(e) for e in raw_args[1:]]
        finally:
            self.current_env = previous_env

        return LambdaNode(params, body, lambda_env)

    def _handle_defun(self, raw_args, ctx) -> ASTNode:
        if len(raw_args) < 3:
            self.collector.add_error(ArityError(
                message="defun requires name, parameters and body",
                span=self._get_span(ctx),
                func_name="defun",
                expected_count=">= 3",
                actual_count=len(raw_args)
            ))
            return NilNode()

        # 1. Name Analysis
        name_node = self.visit(raw_args[0])
        func_name = "anonymous"

        if not isinstance(name_node, SymbolNode):
            self.collector.add_error(TypeMismatchError(
                message="defun function name must be a symbol",
                span=self._get_span(raw_args[0]),
                expected_type="Symbol",
                actual_type=type(name_node).__name__
            ))
        else:
            func_name = name_node.name
            self.global_env.define(func_name, is_function=True)

        # 2. Params Analysis
        with self.collector.context(f"Function '{func_name}'"):
            params_node = self.visit(raw_args[1])
            params = self._extract_params(params_node, raw_args[1])

            func_env = Environment(parent=self.current_env)
            previous_env = self.current_env
            self.current_env = func_env

            body = []
            try:
                for p in params:
                    self.current_env.define(p)

                # 3. Body Analysis
                body = [self.visit(e) for e in raw_args[2:]]
            finally:
                self.current_env = previous_env

        return DefunNode(func_name, params, body)

    def _handle_cond(self, raw_args, parent_ctx) -> CondNode:
        clauses = []
        for i, clause_ctx in enumerate(raw_args):
            with self.collector.context(f"Cond Clause #{i + 1}"):
                node = self.visit(clause_ctx)

                # Logic to parse clauses (list of exprs)
                # Варианты AST: ListNode (если (pred val)), CallNode (если (f x)), NilNode

                pred = None
                body = []

                if isinstance(node, ListNode):
                    if not node.elements:
                        # Empty list () is nil
                        pred = NilNode()
                        body = [NilNode()]
                    else:
                        pred = node.elements[0]
                        body = node.elements[1:]
                elif isinstance(node, CallNode):
                    # ANTLR может распарсить (a b) как CallNode(func=a, args=[b])
                    pred = node.func
                    body = node.args
                elif isinstance(node, PrimCallNode):
                    pred = node
                    body = []
                elif isinstance(node, NilNode):
                    # Пустой список ()
                    pred = NilNode()
                    body = [NilNode()]
                else:
                    # Скаляр, например (cond (t 1)) -> 't' парсится как Atom -> SymbolNode,
                    # но visit вернет TrueNode. Но wait, cond clause должен быть списком.
                    # Если user написал (cond t), то clause_ctx это atom.

                    # Проверка на валидность clause
                    self.collector.add_error(InvalidSyntaxError(
                        message="Cond clause must be a list",
                        span=self._get_span(clause_ctx),
                        expected_syntax="(predicate body...)"
                    ))
                    # Recovery
                    pred = node
                    body = [NilNode()]

                if not body:
                    body = [NilNode()]  # implicit nil return

                clauses.append((pred, body))
        return CondNode(clauses)

    def _handle_progn(self, raw_args) -> PrognNode:
        body = [self.visit(expr) for expr in raw_args]
        return PrognNode(body)

    def _handle_logic(self, op: str, raw_args) -> LogicNode:
        args = [self.visit(expr) for expr in raw_args]
        return LogicNode(op, args)

    def _extract_params(self, params_ast: ASTNode, ctx_for_error) -> List[str]:
        """
        Извлекает список имен параметров, регистрируя ошибки типов.
        Возвращает только валидные символы для продолжения анализа.
        """
        elements = []

        # Flattening AST specific structures
        if isinstance(params_ast, ListNode):
            elements = params_ast.elements
        elif isinstance(params_ast, CallNode):
            elements = [params_ast.func] + params_ast.args
        elif isinstance(params_ast, PrimCallNode):
            # Special case: (defun f (list x) ...) -> 'list' is prim call
            elements = [SymbolNode(params_ast.prim_name)] + params_ast.args
        elif isinstance(params_ast, NilNode):
            elements = []
        elif isinstance(params_ast, SymbolNode):
            # Ошибка: параметры не список
            self.collector.add_error(TypeMismatchError(
                message="Parameters must be a list",
                span=self._get_span(ctx_for_error),
                expected_type="List",
                actual_type="Symbol"
            ))
            return []
        else:
            self.collector.add_error(TypeMismatchError(
                message="Parameters must be a list",
                span=self._get_span(ctx_for_error),
                expected_type="List",
                actual_type=type(params_ast).__name__
            ))
            return []

        params = []
        for i, p in enumerate(elements):
            if isinstance(p, SymbolNode):
                params.append(p.name)
            else:
                self.collector.add_error(TypeMismatchError(
                    message=f"Parameter #{i + 1} must be a symbol",
                    span=self._get_span(ctx_for_error),  # Здесь можно точнее если бы visit возвращал span c нодой
                    expected_type="Symbol",
                    actual_type=type(p).__name__
                ))
        return params

    def _build_quoted_data(self, ctx: lispParser.SexprContext) -> ASTNode:
        if ctx.atom():
            return self._build_quoted_atom(ctx.atom())
        else:
            return self._build_quoted_list(ctx.list_())

    def _build_quoted_atom(self, ctx: lispParser.AtomContext) -> ASTNode:
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
            except:
                return SymbolNode(name)
        if ctx.QUOTE():
            return QuoteNode(self._build_quoted_data(ctx.sexpr()))
        return SymbolNode(ctx.getText())

    def _build_quoted_list(self, ctx: lispParser.ListContext) -> ListNode:
        elements = [self._build_quoted_data(x) for x in ctx.sexpr()]
        return ListNode(elements)