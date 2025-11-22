import unittest
from tests.wasm_compiler_tests.base_wasm_test import WasmCompilerTestCase


class TestWasmMemory(WasmCompilerTestCase):

    def test_cons_car_cdr_basic(self):
        """Проверка базовой механики cons/car/cdr"""
        code = """
        (setq p (cons 10 20))
        (car p)
        """
        # car должен вернуть 10
        self.assert_evaluates(code, 10.0)

        code_cdr = """
        (setq p (cons 10 20))
        (cdr p)
        """
        # cdr должен вернуть 20
        self.assert_evaluates(code_cdr, 20.0)

    def test_nested_list(self):
        """Проверка связанных списков: (1 . (2 . nil))"""
        code = """
        (setq lst (cons 1 (cons 2 0))) 
        (car (cdr lst))
        """
        # cdr lst -> (cons 2 0)
        # car от этого -> 2
        self.assert_evaluates(code, 2.0)

    def test_list_manipulation_func(self):
        """Функция, возвращающая второй элемент списка"""
        code = """
        (defun second (lst)
            (car (cdr lst)))

        (second (cons 100 (cons 200 300)))
        """
        self.assert_evaluates(code, 200.0)

    def test_memory_persistence(self):
        """Проверка, что данные не затираются при новых выделениях"""
        code = """
        (setq a (cons 1 1))
        (setq b (cons 2 2))
        (setq c (cons 3 3))
        (car a) ; Должно остаться 1, несмотря на создание b и c
        """
        self.assert_evaluates(code, 1.0)