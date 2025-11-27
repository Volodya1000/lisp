import unittest
from tests.wasm_compiler_tests.base_wasm_test import WasmCompilerTestCase

class TestScopesAndLet(WasmCompilerTestCase):

    def test_simple_let(self):
        """Verifies basic let binding."""
        code = """
        (defun main ()
            (let ((x 10) (y 5))
                (- x y)))
        (main)
        """
        self.assert_evaluates(code, 5.0)

    def test_nested_let(self):
        """Verifies nested let scopes."""
        code = """
        (defun main ()
            (let ((x 10))
                (let ((y 20))
                    (+ x y))))
        (main)
        """
        self.assert_evaluates(code, 30.0)

    def test_let_shadowing_global(self):
        """Verifies that let variables shadow global variables."""
        code = """
        (setq x 999)
        (defun main ()
            (let ((x 10))
                x)) ;; Should return local x (10), not global x (999)
        (main)
        """
        self.assert_evaluates(code, 10.0)

    def test_let_shadowing_parameters(self):
        """Verifies that let variables shadow function parameters."""
        code = """
        (defun test (a)
            (let ((a 50))
                a))
        (test 1)
        """
        self.assert_evaluates(code, 50.0)

    def test_let_calculation(self):
        """Verifies that let values can be expressions."""
        code = """
        (defun main ()
            (let ((x (+ 5 5)) (y (* 2 3)))
                (+ x y)))
        (main)
        """
        self.assert_evaluates(code, 16.0)