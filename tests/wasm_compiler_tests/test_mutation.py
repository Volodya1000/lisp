import unittest
from tests.wasm_compiler_tests.base_wasm_test import WasmCompilerTestCase


class TestWasmMutation(WasmCompilerTestCase):

    def test_setq_simple(self):
        """Проверка изменения аргумента и возврата значения"""
        code = """
        (defun modify (x) 
            (setq x 100) 
            x)
        (modify 1)
        """
        # Мы передали 1, но внутри setq изменил на 100. Функция возвращает x.
        wat = self.assert_evaluates(code, 100.0)

        # Проверяем использование tee (присвоение + оставление на стеке) и drop
        self.assertIn("local.tee", wat)
        self.assertIn("drop", wat)

    def test_setq_increment(self):
        """Проверка использования переменной в её же обновлении"""
        code = """
        (defun inc (x)
            (setq x (+ x 1))
            x)
        (inc 10)
        """
        self.assert_evaluates(code, 11.0)

    def test_multi_statement_body(self):
        """
        Критически важный тест: проверяет, что результаты промежуточных выражений
        удаляются со стека (drop), иначе стек переполнится или вернется не то значение.
        """
        code = """
        (defun useless-math (x)
            (+ x 1)     ; результат (6) вычисляется и дропается
            (* x 2)     ; результат (10) вычисляется и дропается
            x)          ; возвращается x (5)
        (useless-math 5)
        """
        wat = self.assert_evaluates(code, 5.0)

        # Должно быть 2 drop'а (для + и для *)
        self.assertEqual(wat.count("drop"), 2)

    def test_logic_not(self):
        # (not 0) -> true (1.0)
        wat = self.assert_evaluates("(not 0)", 1.0)
        self.assertIn("f64.eq", wat)

        # (not 123) -> false (0.0)
        self.assert_evaluates("(not 123)", 0.0)


if __name__ == '__main__':
    unittest.main()