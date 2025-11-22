import unittest
from tests.wasm_compiler_tests.base_wasm_test import WasmCompilerTestCase


class TestWasmFunctions(WasmCompilerTestCase):
    def test_simple_defun(self):
        # Функция идентичности
        code = """
        (defun id (x) x)
        (id 42)
        """
        wat = self.assert_evaluates(code, 42.0)

        # Проверка сигнатуры: теперь первым параметром идет $env
        self.assertIn("(func $id (param $env f64) (param f64) (result f64)", wat)

    def test_math_in_function(self):
        # Функция сложения трех чисел
        code = """
        (defun add3 (a b c)
            (+ a (+ b c)))
        (add3 1 2 3)
        """
        wat = self.assert_evaluates(code, 6.0)

        # Проверка сигнатуры: 1 скрытый параметр env + 3 явных
        self.assertIn("(func $add3 (param $env f64) (param f64) (param f64) (param f64)", wat)

    def test_function_call_chain(self):
        # square вызывает mul
        code = """
        (defun mul (a b) (* a b))
        (defun square (x) (mul x x))
        (square 10)
        """
        wat = self.assert_evaluates(code, 100.0)

        # Проверяем, что внутри square происходит вызов (косвенный)
        self.assertIn("call_indirect", wat)