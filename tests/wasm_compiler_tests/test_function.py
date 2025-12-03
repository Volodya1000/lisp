import unittest
from tests.wasm_compiler_tests.base_test import WasmCompilerTestCase

class TestFunctionsAndClosures(WasmCompilerTestCase):

    def test_recursion_factorial(self):
        code = """
            (defun fact (n)
                (cond ((<= n 1) 1)
                      (t (* n (fact (- n 1))))))
            (fact 5)
            """
        self.assert_evaluates(code, 120.0)

    def test_lambda_usage(self):
        self.assert_evaluates("((lambda (x) (+ x 1)) 10)", 11.0)

        self.assert_evaluates("(setq f (lambda (x) (* x x))) (f 5)", 25.0)

        self.assert_evaluates("(setq G 10) ((lambda (x) (+ x G)) 5)", 15.0)

    def test_pass_function_as_argument(self):
        """Передача функции в другую функцию"""
        code = """
            (defun apply-op (f x) (f x))
            (setq my-inc (lambda (a) (+ a 1)))
            (apply-op my-inc 10)
            """
        self.assert_evaluates(code, 11.0)

    def test_return_lambda(self):
        """Возврат функции из функции"""
        code = """
            (defun factory () (lambda () 42))
            (setq f (factory))
            (f)
            """
        self.assert_evaluates(code, 42.0)

    # Замыкания
    def test_mutable_closure(self):
        """Счетчик: классический тест на изменение замкнутой переменной"""
        code = """
            (defun make-counter (start)
                (lambda () 
                    (setq start (+ start 1))
                    start))

            (setq c1 (make-counter 0))
            (c1) ;; -> 1
            (c1) ;; -> 2
            """
        self.assert_evaluates(code, 2.0)

    def test_multiple_closures_independent(self):
        """Два счетчика не должны влиять друг на друга"""
        code = """
            (defun make-counter (start)
                (lambda () (setq start (+ start 1)) start))

            (setq c1 (make-counter 0))
            (setq c2 (make-counter 10))
            (c1) ; 1
            (c2) ; 11
            (c1) ; 2
            """
        self.assert_evaluates(code, 2.0)


if __name__ == '__main__':
    unittest.main()