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

        # Проверка сигнатуры и доступа к локальным переменным
        self.assertIn("(func $id (param f64) (result f64)", wat)
        self.assertIn("local.get 0", wat)

    def test_math_in_function(self):
        # Функция сложения трех чисел
        code = """
        (defun add3 (a b c) 
            (+ a (+ b c)))
        (add3 1 2 3)
        """
        wat = self.assert_evaluates(code, 6.0)

        self.assertIn("(func $add3 (param f64) (param f64) (param f64)", wat)
        # Убеждаемся, что используются все три аргумента
        self.assertIn("local.get 0", wat)
        self.assertIn("local.get 1", wat)
        self.assertIn("local.get 2", wat)

    def test_function_call_chain(self):
        # square вызывает mul
        code = """
        (defun mul (a b) (* a b))
        (defun square (x) (mul x x))
        (square 10)
        """
        wat = self.assert_evaluates(code, 100.0)

        # Проверяем, что внутри square вызывается mul
        self.assertIn("call $mul", wat)
        self.assertIn("func $square", wat)

if __name__ == '__main__':
    unittest.main()