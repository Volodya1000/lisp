from typing import List, Tuple
from gen.lispParser import lispParser
from gen.lispVisitor import lispVisitor
from .ast_nodes import *
from .symbol_table import Environment


class SemanticAnalyzer(lispVisitor):
    """
    Проходит по Parse Tree, строит AST и строит цепочку окружений (Environment),
    определяя для каждой переменной её уровень вложенности и индекс.
    """

    def __init__(self):
        self.global_env = Environment()
        self.current_env = self.global_env
        self._init_primitives()

    def _init_primitives(self):
        for name in Environment.PRIMITIVES:
            self.global_env.define(name, is_function=True)
        for name in Environment.SPECIAL_FORMS:
            self.global_env.define(name, is_function=False)

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
            return QuoteNode(self._build_quoted_data(ctx.sexpr()))
        else:
            raise RuntimeError(f"Unknown atom type: {ctx.getText()}")

    def visitList(self, ctx: lispParser.ListContext) -> ASTNode:
        sexprs = ctx.sexpr()
        if not sexprs:
            return NilNode()

        first_node = self.visit(sexprs[0])

        if isinstance(first_node, SymbolNode) and first_node.name in Environment.SPECIAL_FORMS:
            return self._handle_special_form_lazy(first_node.name, sexprs[1:])

        rest_nodes = [self.visit(e) for e in sexprs[1:]]

        if isinstance(first_node, SymbolNode) and first_node.name in Environment.PRIMITIVES:
            return PrimCallNode(first_node.name, rest_nodes)

        return CallNode(first_node, rest_nodes)

    def _handle_special_form_lazy(self, name: str, raw_args: List[lispParser.SexprContext]) -> ASTNode:
        if name == 'quote':
            return self._handle_quote(raw_args)
        elif name == 'lambda':
            return self._handle_lambda(raw_args)
        elif name == 'cond':
            return self._handle_cond(raw_args)
        elif name == 'setq':
            return self._handle_setq(raw_args)
        elif name == 'defun':
            return self._handle_defun(raw_args)
        elif name == 'progn':
            return self._handle_progn(raw_args)
        elif name == 'and' or name == 'or':
            return self._handle_logic(name, raw_args)
        raise NotImplementedError(f"Special form '{name}' not implemented")

    # --- Handlers (с исправленными сообщениями об ошибках для тестов) ---

    def _handle_quote(self, raw_args) -> QuoteNode:
        if len(raw_args) != 1:
            raise SyntaxError("quote требует 1 аргумент")
        return QuoteNode(self._build_quoted_data(raw_args[0]))

    def _handle_setq(self, raw_args) -> SetqNode:
        if len(raw_args) != 2:
            raise SyntaxError("setq требует 2 аргумента")

        name_node = self.visit(raw_args[0])
        if not isinstance(name_node, SymbolNode):
            raise SyntaxError("Первый аргумент setq должен быть символом")

        var_name = name_node.name
        value_node = self.visit(raw_args[1])

        existing = self.current_env.resolve(var_name)
        if not existing:
            self.current_env.define(var_name)

        return SetqNode(var_name, value_node)

    def _handle_lambda(self, raw_args) -> LambdaNode:
        if len(raw_args) < 1:
            raise SyntaxError("lambda требует параметры и тело")

        params = self._extract_params(self.visit(raw_args[0]))

        lambda_env = Environment(parent=self.current_env)
        previous_env = self.current_env
        self.current_env = lambda_env

        try:
            for p in params:
                self.current_env.define(p)
            body = [self.visit(e) for e in raw_args[1:]]
        finally:
            self.current_env = previous_env

        return LambdaNode(params, body, lambda_env)

    def _handle_defun(self, raw_args) -> DefunNode:
        if len(raw_args) < 3:
            raise SyntaxError("defun требует имя, параметры и тело")

        name_node = self.visit(raw_args[0])
        if not isinstance(name_node, SymbolNode):
            raise SyntaxError("Имя функции в defun должно быть символом")

        func_name = name_node.name
        self.global_env.define(func_name, is_function=True)

        params = self._extract_params(self.visit(raw_args[1]))

        func_env = Environment(parent=self.current_env)
        previous_env = self.current_env
        self.current_env = func_env

        try:
            for p in params:
                self.current_env.define(p)
            body = [self.visit(e) for e in raw_args[2:]]
        finally:
            self.current_env = previous_env

        return DefunNode(func_name, params, body)

    def _handle_cond(self, raw_args) -> CondNode:
        clauses = []
        for clause_ctx in raw_args:
            node = self.visit(clause_ctx)
            if isinstance(node, ListNode):
                if not node.elements: raise SyntaxError("Неверный clause в cond")
                pred = node.elements[0]
                body = node.elements[1:]
            elif isinstance(node, CallNode):
                pred = node.func
                body = node.args
            elif isinstance(node, PrimCallNode):
                pred = node
                body = []
            elif isinstance(node, NilNode):
                 raise SyntaxError("Неверный clause в cond")
            else:
                pred = node
                body = [TrueNode()]

            if not body: body = [NilNode()]
            clauses.append((pred, body))
        return CondNode(clauses)

    def _handle_progn(self, raw_args) -> PrognNode:
        body = [self.visit(expr) for expr in raw_args]
        return PrognNode(body)

    def _handle_logic(self, op: str, raw_args) -> LogicNode:
        args = [self.visit(expr) for expr in raw_args]
        return LogicNode(op, args)

    def _extract_params(self, params_ast) -> List[str]:
        # Формируем плоский список узлов из разных типов AST
        elements = []
        if isinstance(params_ast, ListNode):
            elements = params_ast.elements
        elif isinstance(params_ast, CallNode):
            elements = [params_ast.func] + params_ast.args
        elif isinstance(params_ast, PrimCallNode):
            elements = [SymbolNode(params_ast.prim_name)] + params_ast.args
        elif isinstance(params_ast, NilNode):
            elements = []
        elif isinstance(params_ast, SymbolNode):
             # Тест test_lambda_params_not_list ожидает ошибку "Ожидался список параметров"
             raise SyntaxError("Ожидался список параметров")
        else:
            raise SyntaxError("Ожидался список параметров")

        params = []
        for p in elements:
            if isinstance(p, SymbolNode):
                params.append(p.name)
            else:
                # Тест test_defun_invalid_param_type ожидает ошибку "Параметр должен быть символом"
                raise SyntaxError(f"Параметр должен быть символом")
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
            try: return NumberNode(float(name))
            except: return SymbolNode(name)
        if ctx.QUOTE():
             return QuoteNode(self._build_quoted_data(ctx.sexpr()))
        return SymbolNode(ctx.getText())

    def _build_quoted_list(self, ctx: lispParser.ListContext) -> ListNode:
        elements = [self._build_quoted_data(x) for x in ctx.sexpr()]
        return ListNode(elements)