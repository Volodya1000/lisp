from typing import List, Optional, Tuple
from gen.lisp_grammerVisitor import lisp_grammerVisitor as ANTLRBaseVisitor
from semantic.errors import (
    SemanticError, NameErrorSemantic, DuplicateDeclarationError,
    AttemptToCallNonFunctionError, ArityError, InvalidBindingError,
    InvalidQuoteError
)
from semantic.symbol_info import SymbolInfo, SymbolKind
from semantic.scope import Scope
from semantic.utils import (
    get_position, extract_symbol_name, is_lambda_expression,
    get_child_expressions, extract_integer_literal, is_literal_atom,
    extract_lambda_context
)
from semantic.validators import CallValidator, DivisionByZeroValidator


class SemanticVisitor(ANTLRBaseVisitor):
    def __init__(self, global_scope: Scope):
        self.errors: List[SemanticError] = []
        self._scope_stack: List[Scope] = [global_scope]
        self._call_validator = CallValidator(self.current_scope)

    @property
    def current_scope(self) -> Scope:
        return self._scope_stack[-1]

    def _enter_scope(self) -> Scope:
        """Создает и активирует новый scope."""
        new_scope = Scope(parent=self.current_scope)
        self._scope_stack.append(new_scope)
        return new_scope

    def _exit_scope(self):
        """Восстанавливает предыдущий scope."""
        if len(self._scope_stack) > 1:
            self._scope_stack.pop()

    def _add_error(self, error: SemanticError):
        """Добавляет ошибку в список."""
        self.errors.append(error)

    def _check_duplicate_names(self, names: List[str], nodes: List, context: str = "name") -> None:
        """Проверяет дубликаты имен в списке."""
        seen = set()
        for name, node in zip(names, nodes):
            if name in seen:
                line, col = get_position(node)
                # Специальное сообщение для параметров лямбда
                if context == "parameter name":
                    msg = f"Duplicate parameter '{name}'"
                else:
                    msg = f"Duplicate {context} '{name}'"
                self._add_error(DuplicateDeclarationError(msg, line, col))
            seen.add(name)

    def _analyze_function_body(self, body_ctxs: List):
        """Анализирует тело функции/let в текущем scope."""
        for expr in body_ctxs:
            self.visit(expr)

    # ===================== Visit Methods =====================

    def visitProgram(self, ctx):
        exprs = get_child_expressions(ctx)
        for expr in exprs:
            try:
                self.visit(expr)
            except SemanticError as e:
                self._add_error(e)
        return None

    def visitAtom(self, ctx):
        # Проверка на неопределенный символ
        symbol_name = extract_symbol_name(ctx)
        if symbol_name is not None:
            # ИГНОРИРУЕМ символы-reader-macros (#' :test и т.д.)
            if symbol_name.startswith('#'):
                return self.visitChildren(ctx)  # <-- ДОБАВЬТЕ ЭТУ СТРОКУ

            if self.current_scope.lookup(symbol_name) is None:
                line, col = get_position(ctx)
                self._add_error(NameErrorSemantic(f"Undefined symbol '{symbol_name}'", line, col))

        # Рекурсивный обход дочерних узлов
        return self.visitChildren(ctx)

    def visitQuoteExpr(self, ctx):
        if not get_child_expressions(ctx):
            line, col = get_position(ctx)
            self._add_error(InvalidQuoteError("Invalid quote form (no expression after quote)", line, col))
        return None

    def visitLambdaExpr(self, ctx):
        # 1. Извлекаем параметры
        param_list_ctx = ctx.paramList()
        param_nodes = getattr(param_list_ctx, "SYMBOL", lambda: [])() if param_list_ctx else []
        param_names = [node.getText() for node in param_nodes]

        # 2. Проверяем дубликаты
        self._check_duplicate_names(param_names, param_nodes, "parameter name")

        # 3. Анализ тела в новом scope
        new_scope = self._enter_scope()
        try:
            # Определяем параметры как переменные
            for param in param_names:
                new_scope.define(param, SymbolInfo(param, SymbolKind.VARIABLE))

            # Обновляем валидатор для нового scope
            self._call_validator = CallValidator(new_scope)

            # Анализируем тело
            body_exprs = get_child_expressions(ctx)
            self._analyze_function_body(body_exprs)
        finally:
            self._exit_scope()
            self._call_validator = CallValidator(self.current_scope)

        return None

    def visitLetExpr(self, ctx):
        # 1. Собираем биндинги
        binding_ctxs = getattr(ctx, "binding", lambda: [])() or []

        # 2. Подготовка данных биндингов
        bindings: List[Tuple[str, object, object]] = []  # (name, expr_ctx, symbol_node)
        seen_names = set()

        for binding in binding_ctxs:
            symbol_node = getattr(binding, "SYMBOL", None)
            if symbol_node is None or binding.SYMBOL() is None:
                line, col = get_position(binding)
                self._add_error(InvalidBindingError("Invalid binding: must be (SYMBOL expr)", line, col))
                continue

            name = binding.SYMBOL().getText()
            expr_ctx = getattr(binding, "expr", lambda: None)()

            if expr_ctx is None:
                line, col = get_position(binding)
                self._add_error(InvalidBindingError(f"Binding for '{name}' missing expression", line, col))
                continue

            if name in seen_names:
                line, col = get_position(binding.SYMBOL())
                self._add_error(DuplicateDeclarationError(
                    f"Duplicate binding name '{name}' in let", line, col
                ))
            else:
                seen_names.add(name)
                bindings.append((name, expr_ctx, binding.SYMBOL()))

        # 3. Вычисляем выражения в СТАРОМ scope
        for _, expr_ctx, _ in bindings:
            self.visit(expr_ctx)

        # 4. Создаем новый scope и определяем переменные
        new_scope = self._enter_scope()
        try:
            for name, expr_ctx, _ in bindings:
                # Определяем как функцию, если expr — lambda
                if is_lambda_expression(expr_ctx):
                    lambda_ctx = expr_ctx.lambdaExpr()
                    param_list_ctx = lambda_ctx.paramList()
                    param_nodes = getattr(param_list_ctx, "SYMBOL", lambda: [])() if param_list_ctx else []
                    param_count = len(param_nodes)
                    new_scope.define(name, SymbolInfo(name, SymbolKind.FUNCTION, param_count))
                else:
                    new_scope.define(name, SymbolInfo(name, SymbolKind.VARIABLE))

            # Обновляем валидатор
            self._call_validator = CallValidator(new_scope)

            # 5. Анализируем тело в НОВОМ scope
            body_exprs = get_child_expressions(ctx)
            self._analyze_function_body(body_exprs)
        finally:
            self._exit_scope()
            self._call_validator = CallValidator(self.current_scope)

        return None

    def visitList(self, ctx):
        exprs = get_child_expressions(ctx)
        if not exprs:
            return None

        func_expr = exprs[0]
        args = exprs[1:]

        func_name = extract_symbol_name(func_expr)
        if func_name == "defun":
            return self._analyze_defun(args, ctx)

        # Анализируем само выражение вызова
        self._analyze_callable(func_expr, args, ctx)

        # Анализируем аргументы
        for arg in args:
            self.visit(arg)

        return None

    def _analyze_defun(self, args: List, ctx):
        """Анализирует (defun name (params) body...)."""

        # 1. Проверка количества аргументов
        if len(args) < 3:
            line, col = get_position(ctx)
            self._add_error(ArityError("defun requires name, parameter list, and body", line, col))
            return

        # 2. Имя функции
        name_expr = args[0]
        func_name = extract_symbol_name(name_expr)
        if not func_name:
            line, col = get_position(name_expr)
            self._add_error(InvalidBindingError("defun requires a function name", line, col))
            return

        # 3. Список параметров
        params_expr = args[1]

        # ====== ГЛАВНОЕ ИСПРАВЛЕНИЕ: получаем ListContext из детей ======
        param_list_ctx = None

        # Попробуем получить детей и найти ListContext
        if hasattr(params_expr, 'children') and params_expr.children:
            for child in params_expr.children:
                # child может быть TerminalNode или ParserRuleContext
                child_type = type(child).__name__
                print(f"Child: {child_type} = '{child.getText()}'")

                # Проверяем, является ли child ListContext
                if child_type == 'ListContext':
                    param_list_ctx = child
                    break

        # Если не нашли в children, пробуем через getChild()
        if param_list_ctx is None and hasattr(params_expr, 'getChildCount'):
            for i in range(params_expr.getChildCount()):
                child = params_expr.getChild(i)
                child_type = type(child).__name__
                print(f"getChild({i}): {child_type} = '{child.getText()}'")

                if child_type == 'ListContext':
                    param_list_ctx = child
                    break

        # Окончательная проверка
        if param_list_ctx is None:
            line, col = get_position(params_expr)
            self._add_error(InvalidBindingError("defun requires a parameter list", line, col))
            return

        # 4. Извлекаем параметры из param_list_ctx
        param_names = []
        param_nodes = []

        # Получаем все expr внутри списка параметров
        if hasattr(param_list_ctx, 'expr'):
            param_exprs = param_list_ctx.expr()

            for pexpr in param_exprs:
                # Каждый param_expr - это expr с atom -> SYMBOL
                if hasattr(pexpr, "atom") and pexpr.atom() is not None:
                    atom = pexpr.atom()

                    if hasattr(atom, "SYMBOL") and atom.SYMBOL() is not None:
                        symbol_node = atom.SYMBOL()
                        symbol_name = symbol_node.getText()
                        param_nodes.append(symbol_node)
                        param_names.append(symbol_name)

        if not param_names:
            line, col = get_position(params_expr)
            self._add_error(InvalidBindingError("Parameter list cannot be empty", line, col))
            return

        # 5. Проверяем дубликаты параметров
        self._check_duplicate_names(param_names, param_nodes, "parameter name")

        # 6. Определяем функцию в ГЛОБАЛЬНОМ scope
        global_scope = self._scope_stack[0]
        global_scope.define(func_name, SymbolInfo(func_name, SymbolKind.FUNCTION, len(param_names)))

        # 7. Анализируем тело в новом scope
        new_scope = self._enter_scope()
        try:
            for param in param_names:
                new_scope.define(param, SymbolInfo(param, SymbolKind.VARIABLE))

            self._call_validator = CallValidator(new_scope)

            for body_expr in args[2:]:
                self.visit(body_expr)
        finally:
            self._exit_scope()
            self._call_validator = CallValidator(self.current_scope)

    def _analyze_callable(self, func_ctx, args: List, call_ctx):
        """Анализирует вызываемое выражение и валидирует вызов."""
        # Случай 1: Вызов по имени символа
        if symbol_name := extract_symbol_name(func_ctx):
            return self._analyze_symbol_call(symbol_name, args, call_ctx)

        # Случай 2: Вызов inline lambda
        if is_lambda_expression(func_ctx):
            return self._analyze_lambda_call(func_ctx, args, call_ctx)

        # Случай 3: Попытка вызвать литерал (42, "hello", etc.)
        if is_literal_atom(func_ctx):
            line, col = get_position(func_ctx)
            self._add_error(AttemptToCallNonFunctionError(
                "Attempt to call a literal (not a function)", line, col
            ))
            return

        # Случай 4: Другое выражение (времени выполнения)
        # Например: ((lambda (x) x) 42)
        self.visit(func_ctx)

    def _analyze_symbol_call(self, func_name: str, args: List, ctx):
        """Валидирует вызов функции по имени."""
        symbol = self.current_scope.lookup(func_name)

        if symbol is None:
            line, col = get_position(ctx)
            self._add_error(NameErrorSemantic(f"Undefined function '{func_name}'", line, col))
            return

        if symbol.kind not in (SymbolKind.FUNCTION, SymbolKind.BUILTIN):
            line, col = get_position(ctx)
            self._add_error(AttemptToCallNonFunctionError(
                f"Attempt to call non-function '{func_name}'", line, col
            ))
            return

        # Проверка арности
        if error := self._call_validator.validate_function_call(func_name, args, ctx):
            self._add_error(error)

        # Специфические проверки
        if func_name == "/":
            self._check_division_by_zero(args)

        if func_name == "format":
            if error := self._call_validator.validate_format_call(args, ctx):
                self._add_error(error)

    def _analyze_lambda_call(self, lambda_expr_ctx: object, args: List, ctx):
        """Валидирует вызов inline lambda."""
        # Извлекаем LambdaExprContext из ExprContext
        lambda_ctx = extract_lambda_context(lambda_expr_ctx)
        if lambda_ctx is None:
            return

        # Валидируем арность
        if error := self._call_validator.validate_lambda_call(lambda_ctx, args, ctx):
            self._add_error(error)

        # Анализируем саму lambda (ее тело)
        self.visit(lambda_ctx)

    def _check_division_by_zero(self, args: List):
        """Статическая проверка деления на ноль."""
        validator = DivisionByZeroValidator()
        if error := validator.validate_division(args, get_position):
            self._add_error(error)