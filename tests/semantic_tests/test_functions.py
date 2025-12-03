from semantic.ast_nodes import DefunNode, LambdaNode, CallNode, PrimCallNode, SymbolNode
from semantic.diagnostics import ArityError, TypeMismatchError
from tests.semantic_tests.base_semantic_test import BaseSemanticTest


class TestFunctions(BaseSemanticTest):
    def test_lambda_definition(self):
        nodes, _ = self.parse_and_analyze("(lambda (x y) (+ x y))")
        assert isinstance(nodes[0], LambdaNode)
        assert nodes[0].params == ["x", "y"]
        assert len(nodes[0].body) == 1

    def test_defun_simple(self):
        code = "(defun my-sum (x y) (+ x y))"
        nodes, analyzer = self.parse_and_analyze(code)
        assert isinstance(nodes[0], DefunNode)
        assert nodes[0].name == "my-sum"
        assert analyzer.global_env.resolve("my-sum").is_function is True

    def test_defun_recursion(self):
        """Рекурсивная функция должна видеть сама себя"""
        code = """
        (defun fact (n)
            (if (eq n 0) 1 (* n (fact (- n 1)))))
        """
        nodes, analyzer = self.parse_and_analyze(code)
        assert analyzer.global_env.resolve("fact") is not None

    def test_call_user_function(self):
        code = """
        (defun inc (x) (+ x 1))
        (inc 10)
        """
        nodes, _ = self.parse_and_analyze(code)
        assert isinstance(nodes[1], CallNode)
        assert nodes[1].func.name == "inc"

    def test_call_primitive(self):
        nodes, _ = self.parse_and_analyze("(+ 1 2)")
        assert isinstance(nodes[0], PrimCallNode)

    def test_call_lambda_direct(self):
        nodes, _ = self.parse_and_analyze("((lambda (x) x) 42)")
        assert isinstance(nodes[0], CallNode)
        assert isinstance(nodes[0].func, LambdaNode)

    def test_defun_param_name_edge_case(self):
        """Параметр с именем примитива (list) допустим"""
        nodes, _ = self.parse_and_analyze("(defun my-func (list) list)")
        assert nodes[0].params == ["list"]

    def test_lambda_errors(self):
        # Ошибка: параметры не список
        _, analyzer = self.parse_and_analyze("(lambda x x)")
        assert isinstance(analyzer.collector.errors[0], TypeMismatchError)

    def test_defun_errors(self):
        # Ошибка: имя функции не символ
        _, analyzer = self.parse_and_analyze("(defun 123 (x) x)")
        assert isinstance(analyzer.collector.errors[0], TypeMismatchError)

        # Ошибка: арность (нет тела)
        self.setup_method()
        _, analyzer = self.parse_and_analyze("(defun f (x))")
        assert analyzer.collector.has_errors()