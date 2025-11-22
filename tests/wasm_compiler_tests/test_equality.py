import unittest
from tests.wasm_compiler_tests.base_wasm_test import WasmCompilerTestCase


class TestListsAndQuote(WasmCompilerTestCase):

    def test_list_function(self):
        code = """
        (setq l (list 10 20 30))
        (car (cdr l)) ; 20
        """
        self.assert_evaluates(code, 20.0)

    def test_quote_empty(self):
        self.assert_evaluates("'()", 0.0)
        self.assert_evaluates("(quote ())", 0.0)

    def test_quote_numbers(self):
        code = """
        (setq l '(1 2))
        (car l)
        """
        self.assert_evaluates(code, 1.0)

    def test_quote_nested(self):
        code = """
        (setq l '(1 (2 3)))
        (car (car (cdr l))) ; (2 3) -> car -> 2
        """
        self.assert_evaluates(code, 2.0)

    def test_mixed_creation(self):
        code = """
        (setq l1 (list 1 2))
        (setq l2 (cons 0 l1)) ; (0 1 2)
        (car (cdr (cdr l2)))  ; 2
        """
        self.assert_evaluates(code, 2.0)

    def test_atom_basic(self):
        """
        В MVP 'atom' работает как 'null check' (проверка на пустой список),
        так как мы не можем отличить число от указателя без тегов.
        """
        self.assert_evaluates("(atom '())", 1.0)  # nil is atom
        self.assert_evaluates("(atom nil)", 1.0)  # nil is atom

        # cons-пара точно не nil, значит для нашей логики "не атом" (в смысле не пуста)
        # В полноценном Lisp (atom (cons 1 2)) -> false. У нас это работает так же.
        self.assert_evaluates("(atom '(1 2))", 0.0)