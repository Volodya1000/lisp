import unittest
from tests.wasm_compiler_tests.base_wasm_test import WasmCompilerTestCase

class TestFunctionalLogic(WasmCompilerTestCase):

    def test_map_and_sum(self):
        """
        Reimplements 'map' and 'reduce' logic.
        Creates a list (1 2 3), maps (* x 10) -> (10 20 30), sums them -> 60.
        """
        code = """
        (defun map (f lst)
            (if (eq lst nil)
                nil
                (cons (funcall f (car lst)) 
                      (map f (cdr lst)))))

        (defun sum-list (lst)
            (if (eq lst nil)
                0
                (+ (car lst) (sum-list (cdr lst)))))

        (defun main ()
            (let ((nums (cons 1 (cons 2 (cons 3 nil)))))
                (sum-list (map (lambda (x) (* x 10)) nums))))

        (main)
        """
        self.assert_evaluates(code, 60.0)

    def test_filter_list(self):
        """
        Reimplements 'filter'.
        List (1 2 3 4 5 6). Filter (> x 3) -> (4 5 6). Sum -> 15.
        """
        code = """
        (defun filter (pred lst)
            (cond 
                ((eq lst nil) nil)
                ((funcall pred (car lst)) 
                 (cons (car lst) (filter pred (cdr lst))))
                (t (filter pred (cdr lst)))))

        (defun sum-list (lst)
            (if (eq lst nil) 0 (+ (car lst) (sum-list (cdr lst)))))

        (defun make-list (n max)
            (if (> n max) nil (cons n (make-list (+ n 1) max))))

        (defun main ()
            (let ((nums (make-list 1 6)))
                 (sum-list (filter (lambda (x) (> x 3)) nums))))

        (main)
        """
        self.assert_evaluates(code, 15.0) # 4 + 5 + 6