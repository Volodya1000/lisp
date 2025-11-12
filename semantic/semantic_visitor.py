from typing import Optional, List

from gen.lisp_grammerVisitor import lisp_grammerVisitor as ANTLRBaseVisitor
from semantic.builtin_functions import BUILTIN_FUNCTIONS
from semantic.errors import SemanticError, NameErrorSemantic, DuplicateDeclarationError, AttemptToCallNonFunctionError, \
    ArityError, InvalidBindingError, DivisionByZeroError, InvalidQuoteError
from semantic.utils import _pos_from_ctx, _is_int_literal
from semantic.symbol_info import SymbolInfo
from semantic.scope import Scope


class SemanticVisitor(ANTLRBaseVisitor):
    def __init__(self, global_scope: Optional[Scope] = None):
        self.errors: List[SemanticError] = []
        self.global_scope = global_scope if global_scope is not None else Scope(None)
        # Загружаем встроенные функции в глобальный scope
        if not self.global_scope.symbols:
            for name, meta in BUILTIN_FUNCTIONS.items():
                self.global_scope.symbols[name] = SymbolInfo(name=name, kind="function",
                                                             param_count=meta["param_count"])

    def _add_error(self, err: SemanticError):
        self.errors.append(err)

    def visitProgram(self, ctx):
        exprs = getattr(ctx, "expr", lambda *a: [])()
        for expr in exprs:
            try:
                self.visit(expr)
            except SemanticError as e:
                self._add_error(e)
        return None


    def visitAtom(self, ctx):
        if getattr(ctx, "SYMBOL", None) and ctx.SYMBOL() is not None:
            name = ctx.SYMBOL().getText()
            info = self.global_scope.lookup(name)
            if info is None:
                line, col = _pos_from_ctx(ctx)
                self._add_error(NameErrorSemantic(f"Undefined symbol '{name}'", line, col))
            return None

        # Мы должны посетить дочерние узлы (например, quoteExpr)
        return self.visitChildren(ctx)

    def visitQuoteExpr(self, ctx):
        if ctx is None:
            return None

        expr_val = None
        if hasattr(ctx, "expr"):
            expr_val = ctx.expr()  # Может вернуть Node, List или None

        # Проверяем, что выражение после ' есть
        if expr_val is None or (isinstance(expr_val, list) and not expr_val):
            line, col = _pos_from_ctx(ctx)
            self._add_error(InvalidQuoteError("Invalid quote form (no expression after quote)", line, col))

        # (Если грамматика ' expr)
        # Если тест test_quote_error("'+ 1 2") по-прежнему падает,
        # это, вероятно, СИНТАКСИЧЕСКАЯ ошибка, а не семантическая.
        # Парсер не должен был принимать '+ 1 2.

        # Мы не посещаем self.visit(expr_val), т.к. quote не вычисляет свой аргумент
        return None

    def visitLambdaExpr(self, ctx):
        try:
            param_list_ctx = ctx.paramList()
            param_symbols = []
            param_nodes = []
            if param_list_ctx is not None:
                syms = getattr(param_list_ctx, "SYMBOL", lambda *a: [])()
                param_symbols = [t.getText() for t in syms]
                param_nodes = syms

            seen = set()
            for p_name, p_node in zip(param_symbols, param_nodes):
                if p_name in seen:
                    line, col = _pos_from_ctx(p_node)
                    self._add_error(
                        DuplicateDeclarationError(f"Duplicate parameter name '{p_name}' in lambda", line, col))
                seen.add(p_name)

            new_scope = Scope(parent=self.global_scope)
            for p in param_symbols:
                new_scope.symbols[p] = SymbolInfo(name=p, kind="var")

            old_scope = self.global_scope
            self.global_scope = new_scope

            # Посещаем тело lambda
            exprs = getattr(ctx, "expr", lambda *a: [])()
            for e in exprs:
                self.visit(e)

            self.global_scope = old_scope
        except SemanticError as e:
            self._add_error(e)
        return None

    def visitLetExpr(self, ctx):
        try:
            binding_list = getattr(ctx, "binding", lambda *a: [])()

            # Реализуем "параллельный" let (как в Scheme)
            # 1. Сначала вычисляем ВСЕ выражения в СТАРОМ scope
            # 2. Затем создаем НОВЫЙ scope и связываем в нем ВСЕ переменные

            new_scope = Scope(parent=self.global_scope)
            defined_symbols = {}  # name -> ctx
            binding_data = []  # (name, expr_ctx, name_ctx)

            # 1. Сбор информации и проверка дубликатов
            for b in binding_list:
                sym_node = getattr(b, "SYMBOL", None)
                if sym_node is None or b.SYMBOL() is None:
                    line, col = _pos_from_ctx(b)
                    self._add_error(InvalidBindingError("Invalid binding in let: must be (SYMBOL expr)", line, col))
                    continue

                name = b.SYMBOL().getText()

                expr_ctx_val = None
                if hasattr(b, "expr"):
                    expr_ctx_val = b.expr()  # Get ExprContext

                if expr_ctx_val is None:
                    line, col = _pos_from_ctx(b)
                    self._add_error(InvalidBindingError(f"Invalid binding for '{name}': missing expression", line, col))
                    continue

                if name in defined_symbols:
                    line, col = _pos_from_ctx(b.SYMBOL())
                    self._add_error(
                        DuplicateDeclarationError(f"Duplicate binding name '{name}' in same let", line, col))
                else:
                    defined_symbols[name] = b.SYMBOL()

                binding_data.append((name, expr_ctx_val, b.SYMBOL()))

            # 2. Посещаем (вычисляем) выражения в СТАРОМ scope
            for name, expr_ctx, name_ctx in binding_data:
                self.visit(expr_ctx)

            # 3. Связываем переменные в НОВОМ scope
            for name, expr_ctx, name_ctx in binding_data:
                if not new_scope.exists_in_current(name):
                    # Если выражение — lambda, добавляем как функцию
                    if getattr(expr_ctx, "lambdaExpr", None) and expr_ctx.lambdaExpr() is not None:
                        param_list_ctx = expr_ctx.lambdaExpr().paramList()
                        param_count = len(getattr(param_list_ctx, "SYMBOL", lambda *a: [])())
                        new_scope.symbols[name] = SymbolInfo(
                            name=name,
                            kind="function",
                            param_count=param_count
                        )
                    else:
                        # обычная переменная
                        new_scope.symbols[name] = SymbolInfo(name=name, kind="var")

            # 4. Посещаем тело let в НОВОМ scope
            old_scope = self.global_scope
            self.global_scope = new_scope

            body_exprs = getattr(ctx, "expr", lambda *a: [])()
            for e in body_exprs:
                self.visit(e)

            # 5. Восстанавливаем старый scope
            self.global_scope = old_scope

        except SemanticError as e:
            self._add_error(e)
        return None


    def visitList(self, ctx):
        exprs = getattr(ctx, "expr", lambda *a: [])()
        if not exprs:
            return None  # Пустой список () - это '()', все нормально

        first = exprs[0]
        passed_args = exprs[1:]

        # first = SYMBOL?
        if getattr(first, "atom", None) and first.atom() is not None and \
                getattr(first.atom(), "SYMBOL", None) and first.atom().SYMBOL() is not None:

            func_name = first.atom().SYMBOL().getText()
            info = self.global_scope.lookup(func_name)

            if info is None:
                line, col = _pos_from_ctx(first)
                self._add_error(NameErrorSemantic(f"Undefined function/symbol '{func_name}'", line, col))
            else:
                if info.kind != "function":
                    line, col = _pos_from_ctx(first)
                    self._add_error(
                        AttemptToCallNonFunctionError(f"Attempt to call non-function symbol '{func_name}'", line, col))
                else:
                    # Проверка арности (количества аргументов)
                    if info.param_count is not None:
                        expected = info.param_count
                        got = len(passed_args)
                        if expected != got:
                            line, col = _pos_from_ctx(ctx)
                            self._add_error(
                                ArityError(f"Function '{func_name}' expects {expected} arguments, got {got}", line,
                                           col))

            # Статическая проверка деления на ноль
            if func_name == "/" and len(passed_args) >= 2:
                # Проверяем *второй* аргумент (индекс 1)
                divisor_val = _is_int_literal(passed_args[1])
                if divisor_val == 0:
                    line, col = _pos_from_ctx(passed_args[1])
                    self._add_error(DivisionByZeroError("Division by zero detected (statically)", line, col))

        # first = LITERAL (e.g. 42, "foo")?
        elif getattr(first, "atom", None) and first.atom() is not None:
            line, col = _pos_from_ctx(first)
            self._add_error(AttemptToCallNonFunctionError("Attempt to call a literal (not a function)", line, col))

        # first = (lambda ...)?
        elif getattr(first, "lambdaExpr", None) and first.lambdaExpr() is not None:
            param_list_ctx = first.lambdaExpr().paramList()
            params = []
            if param_list_ctx is not None:
                syms = getattr(param_list_ctx, "SYMBOL", lambda *a: [])()
                params = [t.getText() for t in syms]

            expected = len(params)
            got = len(passed_args)
            if expected != got:
                line, col = _pos_from_ctx(first)
                self._add_error(ArityError(f"Inline lambda expects {expected} args, got {got}", line, col))

            # Должны посетить саму lambda, чтобы проверить ее тело
            self.visit(first)
        else:
            # first - это другое выражение, например (list 1 2)
            # ((list 1 2) 3) - это ошибка времени выполнения, но семантически
            # мы должны просто посетить (list 1 2)
            self.visit(first)

        # Посещаем все аргументы вызова
        for a in passed_args:
            self.visit(a)

        return None
