import unittest
from tests.wasm_compiler_tests.base_wasm_test import WasmCompilerTestCase


class TestRawLists(WasmCompilerTestCase):

    def test_simple_quoted_list(self):
        """Проверка компиляции простого списка '(1 2 3)"""
        # '(1 2 3) -> (1 . (2 . (3 . nil)))
        # car -> 1
        code = "(car '(1 2 3))"
        self.assert_evaluates(code, 1.0)

        # car cdr -> 2
        code = "(car (cdr '(1 2 3)))"
        self.assert_evaluates(code, 2.0)

    def test_nested_quoted_list(self):
        """Проверка вложенных списков '(1 (2 3) 4)"""
        # car (car (cdr '(1 (2 3) 4))) -> car of (2 3) -> 2
        code = """
        (setq L '(1 (2 3) 4))
        (car (car (cdr L)))
        """
        self.assert_evaluates(code, 2.0)

    def test_empty_list_in_quote(self):
        """Проверка пустого списка внутри quote"""
        # '(1 () 2)
        code = """
        (setq L '(1 () 2))
        (atom (car (cdr L))) ;; car of () -> nil, atom nil -> true (1.0)
        """
        self.assert_evaluates(code, 1.0)

    def test_mixed_list_construction(self):
        """Смешивание runtime (cons) и compile-time (quote)"""

        # 1. Проверяем CAR
        code_car = """
        (setq L1 '(2 3))
        (setq L2 (cons 1 L1))
        (car L2)
        """
        self.assert_evaluates(code_car, 1.0)

        # 2. Проверяем CDR
        # ВАЖНО: Мы должны снова определить L1 и L2, так как
        # assert_evaluates запускает компиляцию с чистого листа.
        code_cdr = """
        (setq L1 '(2 3))
        (setq L2 (cons 1 L1))
        (car (cdr L2))
        """
        self.assert_evaluates(code_cdr, 2.0)

    def test_quoted_symbols_as_nil(self):
        """
        Пока символы внутри quote не интернируются, они компилируются в 0.0 (nil),
        но структура списка сохраняется.
        """
        # '(a b c) -> (0.0 . (0.0 . (0.0 . nil)))
        # Длина должна быть 3
        code = """
        (defun len (lst)
            (cond 
                ((atom lst) 0)
                (t (+ 1 (len (cdr lst))))))

        (len '(a b c))
        """
        self.assert_evaluates(code, 3.0)


if __name__ == '__main__':
    unittest.main()