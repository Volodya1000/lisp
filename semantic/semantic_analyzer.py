"""
Семантический анализатор: разрешение символов, построение замыканий
"""
from typing import List, Tuple
from gen.lispParser import lispParser
from gen.lispVisitor import lispVisitor
from .ast_nodes import *
from .symbol_table import Environment, SymbolInfo


class SemanticAnalyzer(lispVisitor):
    """Второй проход: из Parse Tree в обогащенное AST"""

    def __init__(self):
        self.global_env = Environment()
        self.current_env = self.global_env
        self._init_primitives()

    def _init_primitives(self):
        """Инициализировать примитивные функции"""
        for name in Environment.PRIMITIVES:
            self.global_env.define(name, is_function=True)

        for name in Environment.SPECIAL_FORMS:
            self.global_env.define(name, is_function=False)

    # --- Правила грамматики (Visitor) ---

    def visitProgram(self, ctx: lispParser.ProgramContext) -> List[ASTNode]:
        """program: form* EOF;"""
        return [self.visit(form) for form in ctx.form()]

    def visitForm(self, ctx: lispParser.FormContext) -> ASTNode:
        """form: sexpr;"""
        return self.visit(ctx.sexpr())

    def visitSexpr(self, ctx: lispParser.SexprContext) -> ASTNode:
        """sexpr: atom | list;"""
        if ctx.atom():
            return self.visit(ctx.atom())
        else:
            return self.visit(ctx.list_())

    # Внутри SemanticAnalyzer

    def _extract_params(self, params_ast: ASTNode) -> List[str]:
        elements = []
        # Пытаемся "распаковать" структуру, которую вернул visit()
        if isinstance(params_ast, ListNode):
            elements = params_ast.elements
        elif isinstance(params_ast, CallNode):
            # Исправление ошибки парсера: (a b) распознано как вызов
            elements = [params_ast.func] + params_ast.args
        elif isinstance(params_ast, PrimCallNode):
            # Исправление ошибки парсера: (list a) распознано как примитив
            elements = [SymbolNode(params_ast.prim_name)] + params_ast.args
        elif isinstance(params_ast, NilNode):
            elements = []
        else:
            raise SyntaxError(f"Ожидался список параметров, получено: {params_ast}")

        params = []
        for p in elements:
            if isinstance(p, SymbolNode):
                params.append(p.name)
            else:
                raise SyntaxError(f"Параметр должен быть символом: {p}")
        return params

    def visitAtom(self, ctx: lispParser.AtomContext) -> ASTNode:
        """atom: NUMBER | STRING | SYMBOL | QUOTE sexpr | NIL | TRUE;"""
        if ctx.NUMBER():
            return NumberNode(float(ctx.NUMBER().getText()))
        elif ctx.STRING():
            text = ctx.STRING().getText()[1:-1]
            return StringNode(text)
        elif ctx.SYMBOL():
            name = ctx.SYMBOL().getText()
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
            raise RuntimeError(f"Неизвестный тип атома: {ctx.getText()}")

    # --- Helper для QUOTE (Data Mode) ---

    def _build_quoted_data(self, ctx: lispParser.SexprContext) -> ASTNode:
        if ctx.atom():
            return self._build_quoted_atom(ctx.atom())
        else:
            return self._build_quoted_list(ctx.list_())

    def _build_quoted_atom(self, ctx: lispParser.AtomContext) -> ASTNode:
        if ctx.NUMBER():
            return NumberNode(float(ctx.NUMBER().getText()))
        elif ctx.STRING():
            text = ctx.STRING().getText()[1:-1]
            return StringNode(text)
        elif ctx.SYMBOL():
            name = ctx.SYMBOL().getText()
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
            raise RuntimeError(f"Неизвестный атом в quote: {ctx.getText()}")

    def _build_quoted_list(self, ctx: lispParser.ListContext) -> ListNode:
        elements = []
        for sexpr_ctx in ctx.sexpr():
            elements.append(self._build_quoted_data(sexpr_ctx))
        return ListNode(elements)

    # --- Обработка списков и спецформ ---

    def visitList(self, ctx: lispParser.ListContext) -> ASTNode:
        """
        list: '(' sexpr* ')'
        Стратегия: "Peek-ahead".
        1. Посещаем первый элемент (голову).
        2. Если это спецформа -> вызываем ленивый обработчик.
        3. Иначе -> жадно вычисляем аргументы.
        4. Если голова - примитив -> PrimCallNode.
        5. Иначе -> CallNode.
        """
        sexprs = ctx.sexpr()

        # 1. Пустой список -> nil
        if not sexprs:
            return NilNode()

        # 2. Анализируем голову
        first_node = self.visit(sexprs[0])

        # 3. Проверка на спецформу (lazy evaluation args)
        if isinstance(first_node, SymbolNode) and first_node.name in Environment.SPECIAL_FORMS:
            return self._handle_special_form_lazy(first_node.name, sexprs[1:])

        # 4. Обычный вызов (функция или примитив): вычисляем аргументы жадно
        rest_nodes = [self.visit(e) for e in sexprs[1:]]

        # 5. Проверка на примитив (+, -, cons...)
        if isinstance(first_node, SymbolNode) and first_node.name in Environment.PRIMITIVES:
            # Тесты ожидают PrimCallNode для встроенных функций
            return PrimCallNode(first_node.name, rest_nodes)

        # 6. Пользовательская функция
        return CallNode(first_node, rest_nodes)

    def _handle_special_form_lazy(self, name: str, raw_args: List[lispParser.SexprContext]) -> ASTNode:
        if name == 'quote':
            return self._handle_quote_lazy(raw_args)
        elif name == 'lambda':
            return self._handle_lambda_lazy(raw_args)
        elif name == 'cond':
            return self._handle_cond_lazy(raw_args)
        elif name == 'setq':
            return self._handle_setq_lazy(raw_args)
        elif name == 'defun':
            return self._handle_defun_lazy(raw_args)

        raise NotImplementedError(f"Спецформа {name} не реализована")

    # --- Ленивые обработчики ---

    def _handle_quote_lazy(self, raw_args: List[lispParser.SexprContext]) -> QuoteNode:
        if len(raw_args) != 1:
            # Сообщение для теста test_quote_no_args
            raise SyntaxError(f"quote требует 1 аргумент")
        return QuoteNode(self._build_quoted_data(raw_args[0]))

    def _handle_setq_lazy(self, raw_args: List[lispParser.SexprContext]) -> SetqNode:
        if len(raw_args) != 2:
            # Сообщение для test_setq_no_args
            raise SyntaxError("setq требует 2 аргумента")

        name_node = self.visit(raw_args[0])
        if not isinstance(name_node, SymbolNode):
            # Сообщение для test_setq_first_not_symbol
            raise SyntaxError("Первый аргумент setq должен быть символом")

        var_name = name_node.name
        value_node = self.visit(raw_args[1])

        existing_info = self.current_env.resolve(var_name)
        if not existing_info:
            self.current_env.define(var_name, is_function=False)

        return SetqNode(var_name, value_node)

    def _handle_lambda_lazy(self, raw_args: List[lispParser.SexprContext]) -> LambdaNode:
        if len(raw_args) < 2:
            raise SyntaxError("lambda требует параметры и тело")

        # 1. Извлекаем параметры
        params_ast = self.visit(raw_args[0])
        params = self._extract_params(params_ast)

        # 2. Создаем новое окружение для замыкания
        lambda_env = Environment(self.current_env)
        for param in params:
            lambda_env.define(param, is_function=False)

        # 3. Анализируем тело функции в новом окружении
        old_env = self.current_env
        self.current_env = lambda_env
        try:
            body_nodes = [self.visit(expr) for expr in raw_args[1:]]
        finally:
            self.current_env = old_env

        return LambdaNode(params, body_nodes, lambda_env)

    def _handle_cond_lazy(self, raw_args: List[lispParser.SexprContext]) -> CondNode:
        clauses = []

        for clause_ctx in raw_args:
            node = self.visit(clause_ctx)

            pred = None
            body = []

            if isinstance(node, CallNode):
                pred = node.func
                body = node.args
            elif isinstance(node, ListNode):
                if not node.elements:
                    # Сообщение для test_cond_empty_clause
                    raise SyntaxError("Неверный clause в cond")
                pred = node.elements[0]
                body = node.elements[1:]
            elif isinstance(node, NilNode):
                # Сообщение для test_cond_empty_clause
                raise SyntaxError("Неверный clause в cond")
            else:
                raise SyntaxError(f"Неверный clause в cond: {node}")

            if not body:
                body = [NilNode()]

            clauses.append((pred, body))

        return CondNode(clauses)

    # ... методы visit_call, visit_prim_call ... остаются теми же, они нужны для двойной диспетчеризации,
    # если AST будет обходиться снова.
    def visit_call(self, node):
        node.func = node.func.accept(self) if hasattr(node.func, 'accept') else node.func
        node.args = [arg.accept(self) for arg in node.args]
        return node

    def visit_prim_call(self, node):
        node.args = [arg.accept(self) for arg in node.args]
        return node

    def visit_setq(self, node):
        node.value = node.value.accept(self)
        return node

    def visit_cond(self, node):
        new_clauses = []
        for pred, body in node.clauses:
            new_pred = pred.accept(self)
            new_body = [expr.accept(self) for expr in body]
            new_clauses.append((new_pred, new_body))
        node.clauses = new_clauses
        return node

    def visit_lambda(self, node):
        if not node.analyzed:
            old_env = self.current_env
            self.current_env = node.closure_env
            node.body = [expr.accept(self) for expr in node.body]
            self.current_env = old_env
            node.analyzed = True
        return node

    def _handle_defun_lazy(self, raw_args: List[lispParser.SexprContext]) -> DefunNode:
        if len(raw_args) < 3:
            raise SyntaxError("defun требует имя, параметры и тело")

        # 1. Имя функции
        name_node = self.visit(raw_args[0])
        if not isinstance(name_node, SymbolNode):
            raise SyntaxError(f"Имя функции в defun должно быть символом, получено: {name_node}")
        func_name = name_node.name

        # 2. Регистрируем функцию в текущем окружении
        self.current_env.define(func_name, is_function=True)

        # 3. Извлекаем параметры
        params_ast = self.visit(raw_args[1])
        params = self._extract_params(params_ast)

        # 4. Создаем окружение функции и анализируем тело
        func_env = Environment(self.current_env)
        for param in params:
            func_env.define(param, is_function=False)

        old_env = self.current_env
        self.current_env = func_env
        try:
            body_nodes = [self.visit(expr) for expr in raw_args[2:]]
        finally:
            self.current_env = old_env

        return DefunNode(func_name, params, body_nodes)


