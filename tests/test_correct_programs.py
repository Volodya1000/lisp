
from semantic.ast_nodes import *
from tests.base_semantic_test import BaseSemanticTest

class TestCorrectPrograms(BaseSemanticTest):
    """Тесты корректных программ"""

    def test_number_literal(self):
        """Числовой литерал"""
        ast = self.parse_and_analyze("42")
        assert len(ast) == 1
        assert isinstance(ast[0], NumberNode)
        assert ast[0].value == 42

    def test_string_literal(self):
        """Строковый литерал"""
        ast = self.parse_and_analyze('"hello"')
        assert len(ast) == 1
        assert isinstance(ast[0], StringNode)
        assert ast[0].value == "hello"

    def test_quote_list(self):
        """Quote списка"""
        ast = self.parse_and_analyze("'(a b c)")
        assert isinstance(ast[0], QuoteNode)
        assert isinstance(ast[0].expr, ListNode)
        assert len(ast[0].expr.elements) == 3

    def test_lambda_simple(self):
        """Простая лямбда (lambda (x) x)"""
        ast = self.parse_and_analyze("(lambda (x) x)")
        assert isinstance(ast[0], LambdaNode)
        assert ast[0].params == ["x"]
        assert len(ast[0].body) == 1

    def test_primitive_call(self):
        """Вызов примитива (+ 1 2)"""
        ast = self.parse_and_analyze("(+ 1 2)")
        assert isinstance(ast[0], PrimCallNode)
        assert ast[0].prim_name == "+"

    def test_setq_simple(self):
        """Присваивание (setq x 42)"""
        ast = self.parse_and_analyze("(setq x 42)")
        assert isinstance(ast[0], SetqNode)
        assert ast[0].var_name == "x"

    def test_cond_simple(self):
        """Простой cond"""
        ast = self.parse_and_analyze("(cond (t 42))")
        assert isinstance(ast[0], CondNode)
        assert len(ast[0].clauses) == 1

    def test_symbol_resolution_var(self):
        """Чтение переменной после setq"""
        ast, analyzer = self.parse_and_analyze("(setq x 42) x")

        # 1. Проверяем структуру
        assert isinstance(ast[1], SymbolNode)
        assert ast[1].name == "x"

        # 2. СЕМАНТИКА: Проверяем, что x есть в окружении
        symbol_info = analyzer.global_env.resolve("x")
        assert symbol_info is not None
        assert symbol_info.is_function is False

    def test_undefined_function_call(self):
        """Вызов неопределенной функции (должна создать временное определение)"""
        ast = self.parse_and_analyze("(undefined-func 1 2)")
        assert isinstance(ast[0], CallNode)
        # Проверь, что в окружении появилась запись для undefined-func

    def test_primitive_resolution(self):
        """Проверка, что примитивы разрешаются корректно"""
        ast = self.parse_and_analyze("+")
        assert isinstance(ast[0], SymbolNode)
        assert ast[0].name == "+"

    def test_call_user_function(self):
        """Вызов функции, хранящейся в переменной"""
        code = """
        (setq my-add (lambda (x y) (+ x y)))
        (my-add 10 20)
        """
        ast = self.parse_and_analyze(code)
        assert isinstance(ast[1], CallNode)
        assert isinstance(ast[1].func, SymbolNode)
        assert ast[1].func.name == "my-add"

    def test_call_lambda_direct(self):
        """Немедленный вызов lambda ((lambda (x) x) 42)"""
        ast = self.parse_and_analyze("((lambda (x) x) 42)")
        assert isinstance(ast[0], CallNode)
        assert isinstance(ast[0].func, LambdaNode)

    def test_nil_literal(self):
        """Литерал nil"""
        ast = self.parse_and_analyze("nil")
        assert isinstance(ast[0], NilNode)

    def test_true_literal(self):
        """Литерал t"""
        ast = self.parse_and_analyze("t")
        assert isinstance(ast[0], TrueNode)

    def test_empty_list_is_nil(self):
        """Пустой список () должен быть NilNode"""
        ast = self.parse_and_analyze("()")
        assert isinstance(ast[0], NilNode)

    def test_lambda_multiple_body_exprs(self):
        """Лямбда с несколькими выражениями в теле"""
        ast = self.parse_and_analyze("(lambda (x) (setq y x) (+ y 1))")
        assert isinstance(ast[0], LambdaNode)
        assert len(ast[0].body) == 2

    def test_nested_lambdas_closure(self):
        """Вложенные лямбды с захватом переменных"""
        code = "(lambda (x) (lambda (y) (+ x y)))"
        ast = self.parse_and_analyze(code)
        outer = ast[0]
        assert isinstance(outer, LambdaNode)
        assert isinstance(outer.body[0], LambdaNode)
        # Проверь, что inner closure_env видит x

    def test_lambda_params_as_callnode(self):
        """Параметры в виде (x y) а не '(x y)"""
        ast = self.parse_and_analyze("(lambda (x y) (+ x y))")
        assert isinstance(ast[0], LambdaNode)
        assert ast[0].params == ["x", "y"]

    def test_cond_multiple_body_in_clause(self):
        """Cond с несколькими выражениями в clause"""
        ast = self.parse_and_analyze("(cond (t 1 2 3))")
        assert isinstance(ast[0], CondNode)
        assert len(ast[0].clauses[0][1]) == 3

    def test_cond_listnode_clause(self):
        """Cond с clause как ListNode (а не CallNode)"""
        ast = self.parse_and_analyze("(cond ((eq x 1) 'one))")
        assert isinstance(ast[0], CondNode)

    def test_variable_scope_isolation(self):
        """Локальные переменные лямбды не видны снаружи"""
        code = """
        (setq f (lambda (x) (setq local 42) x))
        """
        # Мы анализируем только определение функции, вызов не нужен для проверки статической области видимости
        ast, analyzer = self.parse_and_analyze(code)

        # Проверяем, что 'local' НЕ определен в глобальном окружении
        # (предполагая, что метод resolve возвращает None, если не найдено)
        assert analyzer.global_env.resolve("local") is None
        assert analyzer.global_env.resolve("x") is None

    def test_closure_captures_outer(self):
        """Замыкание захватывает внешние переменные"""
        code = """
        (setq x 10)
        (setq f (lambda (y) (+ x y)))
        """
        ast = self.parse_and_analyze(code)
        # Проверь, что closure_env f видит x

    def test_visit_call_recursively_analyzes_args(self):
        """visit_call должен анализировать аргументы"""
        # Это интеграционный тест, проверяющий рекурсивный анализ
        ast = self.parse_and_analyze("(+ (setq x 1) x)")
        call = ast[0]
        assert isinstance(call, PrimCallNode)
        # Второй аргумент должен быть проанализирован SymbolNode