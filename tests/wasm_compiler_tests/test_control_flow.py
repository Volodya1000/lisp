import unittest
from tests.wasm_compiler_tests.base_wasm_test import WasmCompilerTestCase

class TestWasmControlFlow(WasmCompilerTestCase):

    def test_comparisons(self):
        # Проверяем выполнение: 10 > 5 должно быть true (1.0)
        wat = self.assert_evaluates("(> 10 5)", 1.0)

        # Проверяем генерацию инструкций
        self.assertIn("f64.gt", wat)
        self.assertIn("f64.convert_i32_s", wat)  # Проверка конвертации i32 -> f64

        # Проверяем ложный случай
        self.assert_evaluates("(< 10 5)", 0.0)

    def test_simple_cond(self):
        code = """
        (defun check (x)
            (cond 
                ((> x 10) 100) 
                (t 200)))
        (check 5)
        """
        # 5 не больше 10, должны попасть в ветку 't' -> результат 200
        wat = self.assert_evaluates(code, 200.0)

        # Проверяем структуру if/else в WAT
        self.assertIn("(if (result f64)", wat)
        self.assertIn("(then", wat)
        self.assertIn("(else", wat)

        # Доп. проверка: вызов с числом > 10
        code_true = code.replace("(check 5)", "(check 15)")
        self.assert_evaluates(code_true, 100.0)

    def test_factorial(self):
        """Тест выполнения рекурсивного факториала"""
        code = """
        (defun fact (n)
            (cond 
                ((= n 1) 1)
                (t (* n (fact (- n 1))))))
        (fact 5)
        """
        # 5! = 120
        wat = self.assert_evaluates(code, 120.0)

        # Проверяем ключевые элементы рекурсии
        self.assertIn("func $fact", wat)
        self.assertIn("call $fact", wat)
        self.assertIn("f64.sub", wat)  # декремент n


if __name__ == '__main__':
    unittest.main()