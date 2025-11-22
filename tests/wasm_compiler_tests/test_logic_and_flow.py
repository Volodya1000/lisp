import unittest
from tests.wasm_compiler_tests.base_wasm_test import WasmCompilerTestCase

class TestLogicAndFlow(WasmCompilerTestCase):

    def test_basic_comparisons(self):
        """Проверка базовых операторов сравнения (=, <, >, <=, >=)"""
        # В нашем Lisp: true -> 1.0, false -> 0.0
        self.assert_evaluates("(< 5 10)", 1.0)
        self.assert_evaluates("(> 5 10)", 0.0)
        self.assert_evaluates("(= 10 10)", 1.0)
        self.assert_evaluates("(<= 5 5)", 1.0)
        self.assert_evaluates("(>= 10 5)", 1.0)

    def test_complex_comparisons(self):
        """Сравнение результатов вычислений"""
        self.assert_evaluates("(= (+ 2 2) 4)", 1.0)
        self.assert_evaluates("(> (* 2 5) (- 20 15))", 1.0) # 10 > 5

    def test_logic_not(self):
        """Проверка функции not"""
        # (not nil) -> t
        self.assert_evaluates("(not nil)", 1.0)
        # (not 0) -> t (так как 0.0 это nil)
        self.assert_evaluates("(not 0)", 1.0)
        # (not 123) -> nil
        self.assert_evaluates("(not 123)", 0.0)
        # (not t) -> nil
        self.assert_evaluates("(not t)", 0.0)

    def test_nested_logic(self):
        """Вложенная логика и сравнения"""
        # not (5 > 10) -> not (false) -> true
        self.assert_evaluates("(not (> 5 10))", 1.0)
        # not (10 = 10) -> not (true) -> false
        self.assert_evaluates("(not (= 10 10))", 0.0)

    def test_cond_with_logic(self):
        """Использование сравнений внутри cond"""
        code = """
        (defun check (n)
            (cond
                ((< n 0) -1)
                ((= n 0) 0)
                (t 1)))
        """
        # Компилируем один раз, проверяем разные вызовы (логически, через assert_evaluates каждый раз новый модуль)
        self.assert_evaluates(code + "(check -5)", -1.0)
        self.assert_evaluates(code + "(check 0)", 0.0)
        self.assert_evaluates(code + "(check 42)", 1.0)

    def test_bool_conversion(self):
        """Проверка, что результат сравнения можно использовать в арифметике"""
        # В C/WASM true часто 1. (+ 10 (< 5 10)) -> 10 + 1 -> 11
        self.assert_evaluates("(+ 10 (< 5 10))", 11.0)