from semantic.ast_nodes import *
from tests.semantic_tests.base_semantic_test import BaseSemanticTest


class TestCorrectPrograms(BaseSemanticTest):
    """Тесты корректных программ"""

    def test_number_literal(self):
        """Числовой литерал"""
        nodes, _ = self.parse_and_analyze("42")
        assert len(nodes) == 1
        assert isinstance(nodes[0], NumberNode)
        assert nodes[0].value == 42

    def test_string_literal(self):
        """Строковый литерал"""
        nodes, _ = self.parse_and_analyze('"hello"')
        assert len(nodes) == 1
        assert isinstance(nodes[0], StringNode)
        assert nodes[0].value == "hello"

    def test_quote_list(self):
        """Quote списка"""
        nodes, _ = self.parse_and_analyze("'(a b c)")
        assert isinstance(nodes[0], QuoteNode)
        assert isinstance(nodes[0].expr, ListNode)
        assert len(nodes[0].expr.elements) == 3

    def test_lambda_simple(self):
        """Простая лямбда (lambda (x) x)"""
        nodes, _ = self.parse_and_analyze("(lambda (x) x)")
        assert isinstance(nodes[0], LambdaNode)
        assert nodes[0].params == ["x"]
        assert len(nodes[0].body) == 1

    def test_primitive_call(self):
        """Вызов примитива (+ 1 2)"""
        nodes, _ = self.parse_and_analyze("(+ 1 2)")
        assert isinstance(nodes[0], PrimCallNode)
        assert nodes[0].prim_name == "+"

    def test_setq_simple(self):
        """Присваивание (setq x 42)"""
        nodes, _ = self.parse_and_analyze("(setq x 42)")
        assert isinstance(nodes[0], SetqNode)
        assert nodes[0].var_name == "x"

    def test_cond_simple(self):
        """Простой cond"""
        nodes, _ = self.parse_and_analyze("(cond (t 42))")
        assert isinstance(nodes[0], CondNode)
        assert len(nodes[0].clauses) == 1

    def test_symbol_resolution_var(self):
        """Чтение переменной после setq"""
        nodes, analyzer = self.parse_and_analyze("(setq x 42) x")
        assert isinstance(nodes[1], SymbolNode)
        assert nodes[1].name == "x"
        # Проверяем окружение
        assert analyzer.global_env.resolve("x") is not None

    def test_undefined_function_call(self):
        """Вызов неопределенной функции"""
        nodes, _ = self.parse_and_analyze("(undefined-func 1 2)")
        assert isinstance(nodes[0], CallNode)

    def test_primitive_resolution(self):
        """Проверка, что примитивы разрешаются корректно"""
        nodes, _ = self.parse_and_analyze("+")
        assert isinstance(nodes[0], SymbolNode)
        assert nodes[0].name == "+"

    def test_call_user_function(self):
        """Вызов функции, хранящейся в переменной"""
        code = """
        (setq my-add (lambda (x y) (+ x y)))
        (my-add 10 20)
        """
        nodes, _ = self.parse_and_analyze(code)
        assert isinstance(nodes[1], CallNode)
        assert isinstance(nodes[1].func, SymbolNode)
        assert nodes[1].func.name == "my-add"

    def test_call_lambda_direct(self):
        """Немедленный вызов lambda ((lambda (x) x) 42)"""
        nodes, _ = self.parse_and_analyze("((lambda (x) x) 42)")
        assert isinstance(nodes[0], CallNode)
        assert isinstance(nodes[0].func, LambdaNode)

    def test_nil_literal(self):
        """Литерал nil"""
        nodes, _ = self.parse_and_analyze("nil")
        assert isinstance(nodes[0], NilNode)

    def test_true_literal(self):
        """Литерал t"""
        nodes, _ = self.parse_and_analyze("t")
        assert isinstance(nodes[0], TrueNode)

    def test_empty_list_is_nil(self):
        """Пустой список () должен быть NilNode"""
        nodes, _ = self.parse_and_analyze("()")
        assert isinstance(nodes[0], NilNode)

    def test_lambda_multiple_body_exprs(self):
        """Лямбда с несколькими выражениями в теле"""
        nodes, _ = self.parse_and_analyze("(lambda (x) (setq y x) (+ y 1))")
        assert isinstance(nodes[0], LambdaNode)
        assert len(nodes[0].body) == 2

    def test_nested_lambdas_closure(self):
        """Вложенные лямбды с захватом переменных"""
        code = "(lambda (x) (lambda (y) (+ x y)))"
        nodes, _ = self.parse_and_analyze(code)
        outer = nodes[0]
        assert isinstance(outer, LambdaNode)
        assert isinstance(outer.body[0], LambdaNode)

    def test_lambda_params_as_callnode(self):
        """Параметры в виде (x y) а не '(x y)"""
        nodes, _ = self.parse_and_analyze("(lambda (x y) (+ x y))")
        assert isinstance(nodes[0], LambdaNode)
        assert nodes[0].params == ["x", "y"]

    def test_cond_multiple_body_in_clause(self):
        """Cond с несколькими выражениями в clause"""
        nodes, _ = self.parse_and_analyze("(cond (t 1 2 3))")
        assert isinstance(nodes[0], CondNode)
        # В clause хранится список выражений
        assert len(nodes[0].clauses[0][1]) == 3

    def test_cond_listnode_clause(self):
        """Cond с clause как ListNode (а не CallNode)"""
        nodes, _ = self.parse_and_analyze("(cond ((eq x 1) 'one))")
        assert isinstance(nodes[0], CondNode)

    def test_variable_scope_isolation(self):
        """Локальные переменные лямбды не видны снаружи"""
        code = """
        (setq f (lambda (x) (setq local 42) x))
        """
        nodes, analyzer = self.parse_and_analyze(code)

        # Проверяем, что 'local' не определен в глобальном окружении
        assert analyzer.global_env.resolve("local") is None

    def test_closure_captures_outer(self):
        """Замыкание захватывает внешние переменные"""
        code = """
        (setq x 10)
        (setq f (lambda (y) (+ x y)))
        """
        nodes, analyzer = self.parse_and_analyze(code)

        # setq f -> lambda
        lambda_node = nodes[1].value
        assert isinstance(lambda_node, LambdaNode)

        # Проверка: closure_env лямбды видит x (из global_env)
        assert lambda_node.closure_env.resolve("x") is not None

    def test_visit_call_recursively_analyzes_args(self):
        """visit_call должен анализировать аргументы"""
        nodes, _ = self.parse_and_analyze("(+ (setq x 1) x)")
        call = nodes[0]
        assert isinstance(call, PrimCallNode)
        # Первый аргумент setq, второй symbol
        assert isinstance(call.args[0], SetqNode)
        assert isinstance(call.args[1], SymbolNode)

    def test_defun_simple(self):
        """Проверка определения функции через defun"""
        code = """
        (defun my-sum (x y) 
            (+ x y))
        (my-sum 10 20)
        """
        nodes, analyzer = self.parse_and_analyze(code)

        # Проверка 1-го узла (DefunNode)
        assert isinstance(nodes[0], DefunNode)
        assert nodes[0].name == "my-sum"
        assert nodes[0].params == ["x", "y"]

        # Проверка 2-го узла (CallNode)
        assert isinstance(nodes[1], CallNode)
        assert isinstance(nodes[1].func, SymbolNode)
        assert nodes[1].func.name == "my-sum"

        # Проверка, что функция зарегистрирована в глобальном окружении
        assert analyzer.global_env.resolve("my-sum").is_function is True