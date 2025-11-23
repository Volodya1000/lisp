from semantic.ast_nodes import *
from semantic.diagnostics import ArityError, TypeMismatchError
from tests.semantic_tests.base_semantic_test import BaseSemanticTest
import pytest


class TestDefunSemantics(BaseSemanticTest):
    """Расширенные тесты для defun (корректные случаи)"""

    def test_defun_recursion(self):
        """Тест рекурсивного вызова (факториал)"""
        code = """
        (defun fact (n)
            (cond ((eq n 0) 1)
                  (t (* n (fact (- n 1))))))
        """
        nodes, analyzer = self.parse_and_analyze(code)

        defun_node = nodes[0]
        assert isinstance(defun_node, DefunNode)
        assert defun_node.name == "fact"
        assert analyzer.global_env.resolve("fact").is_function is True

    def test_defun_empty_params(self):
        """Функция без параметров"""
        code = "(defun return-five () 5)"
        nodes, _ = self.parse_and_analyze(code)
        assert isinstance(nodes[0], DefunNode)
        assert nodes[0].params == []

    def test_defun_multiple_body_exprs(self):
        """Несколько выражений в теле функции"""
        code = """
        (defun logic (x)
            (setq y (+ x 1))
            (* y 2))
        """
        nodes, _ = self.parse_and_analyze(code)
        assert isinstance(nodes[0], DefunNode)
        assert len(nodes[0].body) == 2

    def test_defun_param_shadowing(self):
        """Параметр перекрывает глобальную переменную"""
        code = """
        (setq x 100)
        (defun test (x) x)
        """
        nodes, analyzer = self.parse_and_analyze(code)
        defun_node = nodes[1]
        assert defun_node.params == ["x"]

    def test_defun_param_name_is_primitive(self):
        """
        КРИТИЧЕСКИЙ ТЕСТ: параметр с именем примитива (list).
        """
        code = "(defun my-func (list x) list)"
        nodes, _ = self.parse_and_analyze(code)

        assert isinstance(nodes[0], DefunNode)
        assert nodes[0].params == ["list", "x"]
        assert isinstance(nodes[0].body[0], SymbolNode)
        assert nodes[0].body[0].name == "list"


class TestDefunErrors(BaseSemanticTest):
    """Тесты ошибок для defun (адаптированные под Error Collector)"""

    def test_defun_arg_count_error(self):
        """Слишком мало аргументов"""
        # (defun my-func) -> не хватает параметров и тела
        nodes, analyzer = self.parse_and_analyze("(defun my-func)")

        assert analyzer.collector.has_errors()
        error = analyzer.collector.errors[0]

        # Ожидаем ошибку арности
        assert isinstance(error, ArityError)
        assert error.func_name == "defun"

    def test_defun_invalid_name(self):
        """Имя функции не символ"""
        # (defun 123 ...) -> имя должно быть символом
        nodes, analyzer = self.parse_and_analyze("(defun 123 (x) x)")

        assert analyzer.collector.has_errors()
        error = analyzer.collector.errors[0]

        # Ожидаем ошибку типа
        assert isinstance(error, TypeMismatchError)
        assert error.expected_type == "Symbol"
        assert error.actual_type == "NumberNode"

    def test_defun_params_not_list(self):
        """Параметры не являются списком"""
        # (defun f 123 x) -> вместо (x) передано число 123
        nodes, analyzer = self.parse_and_analyze("(defun f 123 x)")

        assert analyzer.collector.has_errors()
        error = analyzer.collector.errors[0]

        assert isinstance(error, TypeMismatchError)
        assert error.expected_type == "List"
        # 123 парсится как NumberNode
        assert error.actual_type == "NumberNode"

    def test_defun_invalid_param_type(self):
        """В списке параметров не символы"""
        # (defun f (x 1) x) -> параметр 1 невалиден
        nodes, analyzer = self.parse_and_analyze("(defun f (x 1) x)")

        assert analyzer.collector.has_errors()
        error = analyzer.collector.errors[0]

        assert isinstance(error, TypeMismatchError)
        assert "must be a symbol" in error.message