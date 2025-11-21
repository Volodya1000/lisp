from tests.wasm_compiler_tests.base_wasm_test import WasmCompilerTestCase

class TestMathOps(WasmCompilerTestCase):

    def test_simple_arithmetic(self):
        """Тестирование базовых арифметических операций по одной"""
        self.assert_evaluates("42", 42.0)
        self.assert_evaluates("(+ 10 20)", 30.0)
        self.assert_evaluates("(/ 10 2)", 5.0)

    def test_nested_expressions(self):
        """Тестирование вложенности"""
        # (1 + 2) * 3 = 9
        wat = self.assert_evaluates("(* (+ 1 2) 3)", 9.0)
        # Проверка структуры WAT все еще возможна, так как метод возвращает строку
        self.assertIn("f64.mul", wat)

    def test_complex_math_parametrized(self):
        test_cases = [
            # (Код Lisp, Ожидаемый результат)
            ("(- 10 4)", 6.0),
            ("(* 5 5)", 25.0),
            ("(/ 100 10)", 10.0),
            ("(+ (* 2 2) (* 3 3))", 13.0),  # 4 + 9
            ("(/ (+ 10 20) (- 10 5))", 6.0),  # 30 / 5
            ("(* (+ 1 2) (+ 3 4))", 21.0),  # 3 * 7
        ]

        for code, expected in test_cases:
            with self.subTest(code=code, expected=expected):
                self.assert_evaluates(code, expected)

