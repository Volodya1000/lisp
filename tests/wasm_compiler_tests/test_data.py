import unittest
from tests.wasm_compiler_tests.base_test import WasmCompilerTestCase


class TestDataStructures(WasmCompilerTestCase):

    def test_lists_mechanics(self):
        """Cons, Car, Cdr"""
        # Простая пара
        self.assert_evaluates("(car (cons 10 20))", 10.0)
        self.assert_evaluates("(cdr (cons 10 20))", 20.0)

        # Список (1 2 3) -> (1 . (2 . (3 . nil)))
        code_list = """
        (setq l (cons 1 (cons 2 (cons 3 nil))))
        (car (cdr l))
        """
        self.assert_evaluates(code_list, 2.0)

        # Helper 'list'
        self.assert_evaluates("(car (cdr (list 10 20 30)))", 20.0)

    def test_quote(self):
        """Quote и атомы"""
        self.assert_evaluates("(car '(1 2))", 1.0)
        self.assert_evaluates("(car (cdr '(1 2)))", 2.0)
        self.assert_evaluates("(atom '())", 1.0)
        self.assert_evaluates("(atom '(1 2))", 0.0)
        # Вложенные списки
        self.assert_evaluates("(car (car (cdr '(1 (2 3) 4))))", 2.0)

    def test_strings(self):
        """Строки и вывод (princ)"""
        # Длина строки
        self.assert_evaluates('(length "hello")', 5.0)

        # Конкатенация
        code_concat = """
        (setq s (str-concat "He" "llo"))
        (length s)
        """
        self.assert_evaluates(code_concat, 5.0)

        code_princ = """
        (princ "Line 1")
        (princ "Line 2")
        """
        self.assert_evaluates(code_princ, 0.0)  # princ возвращает 0
        self.assertEqual(self.output_buffer, ["Line 1", "Line 2"])


if __name__ == '__main__':
    unittest.main()