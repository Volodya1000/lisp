import unittest
from tests.wasm_compiler_tests.base_wasm_test import WasmCompilerTestCase


class TestNewFeatures(WasmCompilerTestCase):

    def test_progn_basic(self):
        """Progn должен возвращать последнее значение"""
        code = "(progn 1 2 3)"
        self.assert_evaluates(code, 3.0)

    def test_progn_execution_flow(self):
        """Все выражения внутри progn должны выполняться"""
        code = """
        (setq x 10)
        (progn
            (setq x (+ x 5))
            (setq x (+ x 5))
            x)
        """
        self.assert_evaluates(code, 20.0)

    def test_progn_nested(self):
        """Вложенные progn"""
        self.assert_evaluates("(progn (progn 1 2) 3)", 3.0)

    def test_logic_and(self):
        """AND с использованием nil/t и чисел"""
        self.assert_evaluates("(and 1 2)", 2.0)
        self.assert_evaluates("(and t 10)", 10.0)
        self.assert_evaluates("(and 1 nil)", 0.0) # nil -> 0.0
        self.assert_evaluates("(and nil 5)", 0.0)

    def test_logic_or(self):
        """OR с использованием nil/t и чисел"""
        self.assert_evaluates("(or 1 2)", 1.0)
        self.assert_evaluates("(or nil 5)", 5.0)
        self.assert_evaluates("(or nil nil 10)", 10.0)

    def test_short_circuit_and(self):
        """AND не должен выполнять второй аргумент, если первый false"""
        code = """
        (setq check 0)
        (and nil (setq check 1))
        check
        """
        self.assert_evaluates(code, 0.0)

    def test_short_circuit_or(self):
        """OR не должен выполнять второй аргумент, если первый true"""
        code = """
        (setq check 0)
        (or 1 (setq check 1))
        check
        """
        self.assert_evaluates(code, 0.0)

    def test_string_length(self):
        """length для строк"""
        self.assert_evaluates('(length "hello")', 5.0)
        self.assert_evaluates('(length "")', 0.0)

    def test_string_concat(self):
        """str-concat объединяет строки"""
        # Проверяем через длину, т.к. пока не можем вернуть строку наружу
        code = """
        (setq s1 "Hello, ")
        (setq s2 "World!")
        (length (str-concat s1 s2))
        """
        self.assert_evaluates(code, 13.0)


if __name__ == '__main__':
    unittest.main()