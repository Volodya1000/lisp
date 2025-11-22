import unittest
from tests.wasm_compiler_tests.base_wasm_test import WasmCompilerTestCase

class TestWasmControlFlow(WasmCompilerTestCase):

    def test_comparisons(self):
        self.assert_evaluates("(< 5 10)", 1.0)
        self.assert_evaluates("(> 5 10)", 0.0)
        self.assert_evaluates("(= 10 10)", 1.0)
        self.assert_evaluates("(= 10 11)", 0.0)

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

        # Проверяем, что функция сгенерирована
        self.assertIn("func $fact", wat)
        # В новой архитектуре вызов происходит косвенно через замыкание
        self.assertIn("call_indirect", wat)

if __name__ == '__main__':
    unittest.main()