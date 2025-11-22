import unittest
from tests.wasm_compiler_tests.base_wasm_test import WasmCompilerTestCase


class TestLambdas(WasmCompilerTestCase):

    def test_immediate_lambda_call(self):
        """Вызов лямбды сразу после определения: ((lambda (x) (+ x 1)) 10)"""
        code = "((lambda (x) (+ x 1)) 10)"
        self.assert_evaluates(code, 11.0)

    def test_lambda_in_variable(self):
        """Сохранение лямбды в переменную и вызов через переменную"""
        code = """
        (setq square (lambda (x) (* x x)))
        (square 5)
        """
        self.assert_evaluates(code, 25.0)

    def test_lambda_multiple_args(self):
        """Лямбда с несколькими аргументами"""
        code = "((lambda (x y z) (+ x (+ y z))) 1 2 3)"
        self.assert_evaluates(code, 6.0)

    def test_lambda_access_global(self):
        """Лямбда должна видеть глобальные переменные"""
        code = """
        (setq G 100)
        (setq adder (lambda (x) (+ x G)))
        (adder 5)
        """
        self.assert_evaluates(code, 105.0)

    def test_pass_lambda_as_argument(self):
        """Передача функции как аргумента (Higher Order Function)"""
        # Мы создаем функцию apply-op, которая принимает функцию f и значение x
        # И применяет f к x.
        code = """
        (defun apply-op (f x) (f x))
        (setq my-inc (lambda (a) (+ a 1)))
        (apply-op my-inc 10)
        """
        self.assert_evaluates(code, 11.0)

    def test_return_lambda_from_function(self):
        """Возврат лямбды из функции (без замыкания локальных переменных)"""
        # factory возвращает функцию, которая всегда возвращает 42
        code = """
        (defun factory () 
            (lambda () 42))

        (setq f (factory))
        (f)
        """
        self.assert_evaluates(code, 42.0)

    def test_nested_lambda_calls(self):
        """((lambda (x) (lambda (y) (+ x y))) 10) -- Не сработает без замыканий!"""
        # В текущей реализации это должно упасть или работать некорректно,
        # так как внутренняя лямбда не увидит x.
        # Проверим то, что должно работать: вложенные вызовы
        code = """
        (setq add (lambda (x y) (+ x y)))
        (add (add 1 2) 3)
        """
        self.assert_evaluates(code, 6.0)


if __name__ == '__main__':
    unittest.main()