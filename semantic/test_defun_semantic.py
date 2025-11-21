from semantic.ast_nodes import *
from tests.base_semantic_test import BaseSemanticTest
import pytest


class TestDefunSemantics(BaseSemanticTest):
    """Расширенные тесты для defun"""

    def test_defun_recursion(self):
        """Тест рекурсивного вызова (факториал)"""
        code = """
        (defun fact (n)
            (cond ((eq n 0) 1)
                  (t (* n (fact (- n 1))))))
        """
        nodes, analyzer = self.parse_and_analyze(code)

        # Проверяем, что внутри тела функции 'fact' разрешается корректно
        defun_node = nodes[0]
        # Внутри cond -> clause -> call -> func
        # Это глубокая проверка, но главное, что анализ прошел без ошибок
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

        # Проверяем, что тело функции анализировалось в окружении, где x - локальный
        # (Это косвенная проверка, в AST мы увидим SymbolNode,
        # но семантический анализатор не должен был упасть)
        assert defun_node.params == ["x"]

    def test_defun_param_name_is_primitive(self):
        """
        КРИТИЧЕСКИЙ ТЕСТ для вашей логики: elif isinstance(params_ast, PrimCallNode).
        Если параметры (list a), парсер может подумать, что это вызов функции list.
        Defun должен понять, что 'list' это имя параметра.
        """
        code = "(defun my-func (list x) list)"
        nodes, _ = self.parse_and_analyze(code)

        assert isinstance(nodes[0], DefunNode)
        # Проверяем, что list попал в параметры, а не остался вызовом
        assert nodes[0].params == ["list", "x"]
        # Проверяем тело: это должен быть символ 'list', а не примитив list
        assert isinstance(nodes[0].body[0], SymbolNode)
        assert nodes[0].body[0].name == "list"


class TestDefunErrors(BaseSemanticTest):
    """Тесты ошибок для defun"""

    def test_defun_arg_count_error(self):
        """Слишком мало аргументов"""
        with pytest.raises(SyntaxError, match="defun требует имя"):
            self.parse_and_analyze("(defun my-func)")

    def test_defun_invalid_name(self):
        """Имя функции не символ"""
        with pytest.raises(SyntaxError, match="Имя функции.*должно быть символом"):
            self.parse_and_analyze("(defun 123 (x) x)")

    def test_defun_params_not_list(self):
        """Параметры не являются списком"""
        with pytest.raises(SyntaxError, match="Параметры defun должны быть списком"):
            self.parse_and_analyze("(defun f 123 x)")

    def test_defun_invalid_param_type(self):
        """В списке параметров не символы"""
        with pytest.raises(SyntaxError, match="Параметр defun должен быть символом"):
            self.parse_and_analyze("(defun f (x 1) x)")