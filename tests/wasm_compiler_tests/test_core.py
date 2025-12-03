import unittest
from tests.wasm_compiler_tests.base_test import WasmCompilerTestCase


class TestCoreFeatures(WasmCompilerTestCase):

    def test_arithmetic(self):
        """Базовая арифметика и вложенность"""
        cases = [
            ("42", 42.0),
            ("(+ 10 20)", 30.0),
            ("(- 10 4)", 6.0),
            ("(* 5 5)", 25.0),
            ("(/ 100 10)", 10.0),
            ("(* (+ 1 2) 3)", 9.0),
            ("(/ (+ 10 20) (- 10 5))", 6.0),
        ]
        for code, expected in cases:
            with self.subTest(code=code):
                self.assert_evaluates(code, expected)

    def test_logic_and_comparison(self):
        """Сравнения и логические операторы"""
        self.assert_evaluates("(< 5 10)", 1.0)
        self.assert_evaluates("(> 5 10)", 0.0)
        self.assert_evaluates("(= 10 10)", 1.0)
        self.assert_evaluates("(>= 10 5)", 1.0)

        # Not
        self.assert_evaluates("(not nil)", 1.0)
        self.assert_evaluates("(not 0)", 1.0)  # 0.0 treated as nil
        self.assert_evaluates("(not 123)", 0.0)

        # And / Or
        self.assert_evaluates("(and 1 2)", 2.0)
        self.assert_evaluates("(or 1 2)", 1.0)
        self.assert_evaluates("(or nil 5)", 5.0)

    def test_control_flow(self):
        """Cond, If, Progn"""
        # Cond
        code_cond = """
        (defun check (n)
            (cond ((< n 0) -1) ((= n 0) 0) (t 1)))
        """
        self.assert_evaluates(code_cond + "(check -5)", -1.0)
        self.assert_evaluates(code_cond + "(check 0)", 0.0)
        self.assert_evaluates(code_cond + "(check 10)", 1.0)

        # Progn
        self.assert_evaluates("(progn 1 2 3)", 3.0)
        self.assert_evaluates("(progn (setq x 10) (+ x 1))", 11.0)

    def test_variables_and_scope(self):
        self.assert_evaluates("(setq x 10) x", 10.0)
        self.assert_evaluates("(defun inc (v) (setq v (+ v 1)) v) (inc 10)", 11.0)

        code_let = """
        (setq x 999)
        (defun main ()
            (let ((x 10) (y 20))
                (+ x y)))
        (main)
        """
        self.assert_evaluates(code_let, 30.0)  # local x=10 shadows global x=999

    def test_runtime_errors(self):
        """Ошибки времени выполнения"""
        # Вызов числа как функции
        with self.assertRaises(Exception):
            self.assert_evaluates("(funcall 123 10)", 0.0)


if __name__ == '__main__':
    unittest.main()