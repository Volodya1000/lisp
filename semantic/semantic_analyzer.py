"""
Семантический анализатор: разрешение символов, построение замыканий
"""
from typing import List
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

    # --- Правила грамматики ---

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

    def visitAtom(self, ctx: lispParser.AtomContext) -> ASTNode:
        """atom: NUMBER | STRING | SYMBOL | QUOTE sexpr | NIL | TRUE;"""
        if ctx.NUMBER():
            return NumberNode(float(ctx.NUMBER().getText()))
        elif ctx.STRING():
            text = ctx.STRING().getText()[1:-1]
            return StringNode(text)
        elif ctx.SYMBOL():
            name = ctx.SYMBOL().getText()
            # ВАЖНО: различаем nil и t как специальные литералы
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

    def _build_quoted_data(self, ctx: lispParser.SexprContext) -> ASTNode:
        """Построить литеральные данные из sexpr внутри quote (рекурсивно)"""
        if ctx.atom():
            return self._build_quoted_atom(ctx.atom())
        else:
            return self._build_quoted_list(ctx.list_())

    def _build_quoted_atom(self, ctx: lispParser.AtomContext) -> ASTNode:
        """Построить литеральный атом (без разрешения символов)"""
        if ctx.NUMBER():
            return NumberNode(float(ctx.NUMBER().getText()))
        elif ctx.STRING():
            text = ctx.STRING().getText()[1:-1]
            return StringNode(text)
        elif ctx.SYMBOL():
            name = ctx.SYMBOL().getText()
            # В quote контексте тоже nil и t -> литералы
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
            # Вложенный quote: ''expr
            return QuoteNode(self._build_quoted_data(ctx.sexpr()))
        else:
            raise RuntimeError(f"Неизвестный атом в quote: {ctx.getText()}")
    def _build_quoted_list(self, ctx: lispParser.ListContext) -> ListNode:
        """Построить литеральный список"""
        elements = []
        for sexpr_ctx in ctx.sexpr():
            elements.append(self._build_quoted_data(sexpr_ctx))
        return ListNode(elements)

    def visitList(self, ctx: lispParser.ListContext) -> ASTNode:
        """list: '(' sexpr* ')'"""
        elements = [self.visit(e) for e in ctx.sexpr()]

        # Пустой список -> nil
        if not elements:
            return NilNode()

        # Проверяем, является ли первый элемент символом (спецформа или вызов)
        first = elements[0]
        if isinstance(first, SymbolNode):
            return self._handle_special_form(first.name, elements[1:], ctx)

        # Иначе это вызов функции
        return CallNode(first, elements[1:])

    def _handle_special_form(self, name: str, args: List[ASTNode], ctx) -> ASTNode:
        """Обработка специальных форм и примитивов"""

        # Специальные формы
        if name == 'quote':
            return self._handle_quote(args)
        elif name == 'lambda':
            return self._handle_lambda(args)
        elif name == 'cond':
            return self._handle_cond(args)
        elif name == 'setq':
            return self._handle_setq(args)

        # Встроенные примитивные функции
        elif name in Environment.PRIMITIVES:
            return PrimCallNode(name, args)

        # Пользовательская функция или переменная
        else:
            func_info = self.current_env.resolve(name)
            if not func_info:
                # Необъявленная функция — создаем символ, позже будет ошибка
                func_info = self.current_env.get_global().define(name, is_function=True)

            return CallNode(SymbolNode(name), args)

    def _handle_quote(self, args: List[ASTNode]) -> ASTNode:
        """(quote expr)"""
        if len(args) != 1:
            raise SyntaxError(f"quote требует 1 аргумент, получено {len(args)}")
        return QuoteNode(args[0])

    def _handle_lambda(self, args: List[ASTNode]) -> LambdaNode:
        """(lambda (params) body+)"""
        if len(args) < 2:
            raise SyntaxError(f"lambda требует параметры и тело")

        # Параметры: должны быть списком символов
        params_ast = args[0]
        params = []

        # Извлекаем элементы параметров в зависимости от типа
        if isinstance(params_ast, ListNode):
            # Форма '(x y z) - список как данные
            elements = params_ast.elements
        elif isinstance(params_ast, CallNode):
            # Форма (x y z) - парсится как вызов, но в контексте лямбды это список
            # Собираем все элементы: функция + аргументы
            elements = [params_ast.func] + params_ast.args
        elif isinstance(params_ast, NilNode):
            # Форма () - пустой список параметров, это валидно!
            elements = []  # Просто пустой список
        else:
            raise SyntaxError(f"Параметры lambda должны быть списком, получено {type(params_ast)}")

        # Валидируем каждый элемент: должен быть символом
        for p in elements:
            if isinstance(p, SymbolNode):
                params.append(p.name)
            else:
                raise SyntaxError(f"Параметр {p} ({type(p).__name__}) должен быть символом")

        # Тело: одно или более выражений
        body = args[1:]

        # Создаем новое окружение для тела лямбды
        lambda_env = Environment(self.current_env)
        for param in params:
            lambda_env.define(param, is_function=False)

        # Анализируем тело в новом окружении
        old_env = self.current_env
        self.current_env = lambda_env
        analyzed_body = [expr.accept(self) for expr in body]
        self.current_env = old_env

        return LambdaNode(params, analyzed_body, lambda_env)

    def _handle_cond(self, args: List[ASTNode]) -> CondNode:
        """(cond (pred1 body1...) (pred2 body2...) ... )"""
        clauses = []

        for clause in args:
            # ИСПРАВЛЕНИЕ: clause может быть CallNode (как у вас) или ListNode
            if isinstance(clause, CallNode):
                # (t 42) парсится как CallNode(Symbol('t'), [Number(42)])
                pred = clause.func
                body = clause.args
            elif isinstance(clause, ListNode):
                if len(clause.elements) < 1:
                    raise SyntaxError(f"Неверный clause в cond: {clause}")
                pred = clause.elements[0]
                body = clause.elements[1:] if len(clause.elements) > 1 else [NilNode()]
            else:
                raise SyntaxError(f"Неверный clause в cond: {clause}")

            clauses.append((pred, body))

        return CondNode(clauses)

    def _handle_setq(self, args: List[ASTNode]) -> SetqNode:
        """(setq var value)"""
        if len(args) != 2:
            raise SyntaxError(f"setq требует 2 аргумента, получено {len(args)}")

        if not isinstance(args[0], SymbolNode):
            raise SyntaxError(f"Первый аргумент setq должен быть символом")

        var_name = args[0].name
        value = args[1]

        # Определяем в глобальном окружении
        self.current_env.get_global().define(var_name, is_function=False)

        return SetqNode(var_name, value)

    def visit_symbol(self, node: SymbolNode) -> SymbolNode:
        return node

    def visit_number(self, node: NumberNode) -> NumberNode:
        return node

    def visit_string(self, node: StringNode) -> StringNode:
        return node

    def visit_nil(self, node: NilNode) -> NilNode:
        return node

    def visit_true(self, node: TrueNode) -> TrueNode:
        return node

    def visit_quote(self, node: QuoteNode) -> QuoteNode:
        return node

    def visit_list(self, node: ListNode) -> ListNode:
        return node

    def visit_call(self, node: CallNode) -> CallNode:
        node.func = self.visit(node.func) if hasattr(node.func, 'accept') else node.func
        node.args = [arg.accept(self) for arg in node.args]
        return node

    def visit_prim_call(self, node: PrimCallNode) -> PrimCallNode:
        node.args = [arg.accept(self) for arg in node.args]
        return node

    def visit_setq(self, node: SetqNode) -> SetqNode:
        node.value = node.value.accept(self)
        return node

    def visit_cond(self, node: CondNode) -> CondNode:
        new_clauses = []
        for pred, body in node.clauses:
            new_pred = pred.accept(self)
            new_body = [expr.accept(self) for expr in body]
            new_clauses.append((new_pred, new_body))
        node.clauses = new_clauses
        return node

    def visit_lambda(self, node: LambdaNode) -> LambdaNode:
        if not node.analyzed:
            old_env = self.current_env
            self.current_env = node.closure_env
            node.body = [expr.accept(self) for expr in node.body]
            self.current_env = old_env
            node.analyzed = True
        return node