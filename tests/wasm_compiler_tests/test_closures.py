import unittest
from tests.wasm_compiler_tests.base_wasm_test import WasmCompilerTestCase


class TestClosures(WasmCompilerTestCase):

    def test_simple_closure(self):
        """Тест доступа к переменной из внешнего скоупа"""
        code = """
        (defun make-adder (x)
            (lambda (y) (+ x y)))

        (setq add5 (make-adder 5))
        (funcall add5 10)
        """
        # Примечание: funcall здесь реализован через обычный вызов списка (call_node)
        # Так как в лиспе (add5 10) вызывает то, что лежит в символе add5

        code_direct = """
        (defun make-adder (x)
            (lambda (y) (+ x y)))
        (setq add5 (make-adder 5))
        (add5 10)
        """
        self.assert_evaluates(code_direct, 15.0)

    def test_counter(self):
        """Классический тест счетчика: мутация замкнутой переменной"""
        # Примечание: setq внутри лямбды должен обновлять переменную в хипе
        code = """
        (defun make-counter (start)
            (lambda () 
                (setq start (+ start 1))
                start))

        (setq c1 (make-counter 0))
        (c1) ;; -> 1
        (c1) ;; -> 2
        """
        # Т.к. run_wasm возвращает результат последнего выражения
        self.assert_evaluates(code, 2.0)

    def test_multiple_closures(self):
        """Два разных счетчика не должны влиять друг на друга"""
        code = """
        (defun make-counter (start)
            (lambda () 
                (setq start (+ start 1))
                start))

        (setq c1 (make-counter 0)) ;; 0 -> 1
        (setq c2 (make-counter 10)) ;; 10 -> 11

        (c1) ;; 1
        (c2) ;; 11
        (c1) ;; 2
        """
        self.assert_evaluates(code, 2.0)

    def test_nested_lambdas(self):
        """Вложенность глубже 1 уровня"""
        code = """
        (defun adder-gen (x)
            (lambda (y)
                (lambda (z)
                    (+ x (+ y z)))))

        (setq add-1 (adder-gen 1))
        (setq add-1-2 (add-1 2))
        (add-1-2 3) ;; 1 + 2 + 3 = 6
        """
        self.assert_evaluates(code, 6.0)