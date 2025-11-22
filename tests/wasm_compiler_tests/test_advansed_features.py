import unittest
from tests.wasm_compiler_tests.base_wasm_test import WasmCompilerTestCase


class TestAdvancedFeatures(WasmCompilerTestCase):

    def test_global_variables(self):
        """Проверка чтения и записи глобальных переменных"""
        code = """
        (setq counter 10)
        (defun increment () 
            (setq counter (+ counter 1)))

        (increment)
        (increment)
        """
        # Начали с 10, дважды увеличили -> 12
        # Последнее выражение (increment) вернет новое значение
        wat = self.assert_evaluates(code, 12.0)

        self.assertIn("(global $counter (mut f64)", wat)
        self.assertIn("global.get $counter", wat)
        self.assertIn("global.set $counter", wat)

    def test_global_modification_inside_function(self):
        """Проверка влияния функции на глобальное состояние"""
        code = """
        (setq x 5)
        (defun change-x (new-val)
            (setq x new-val))

        (change-x 42)
        x  ; Возвращаем x, чтобы проверить, что оно изменилось
        """
        self.assert_evaluates(code, 42.0)

    def test_recursion_factorial(self):
        """Классический тест на рекурсию: Факториал"""
        code = """
        (defun fact (n)
            (cond 
                ((<= n 1) 1)
                (t (* n (fact (- n 1))))))

        (fact 5)
        """
        # 5! = 120
        self.assert_evaluates(code, 120.0)

    def test_recursion_fibonacci(self):
        """Тест на двойную рекурсию: Фибоначчи"""
        code = """
        (defun fib (n)
            (cond
                ((<= n 1) n)
                (t (+ (fib (- n 1)) 
                      (fib (- n 2))))))

        (fib 10)
        """
        # Fib(10) = 55
        # 0 1 1 2 3 5 8 13 21 34 55
        self.assert_evaluates(code, 55.0)

    def test_shadowing_global_arg(self):
        """Аргумент функции должен затенять глобальную переменную"""
        code = """
        (setq x 100)
        (defun check-shadow (x)
            (+ x 1)) ; Здесь x должен быть аргументом, а не глобальным

        (check-shadow 10)
        """
        # Если бы брался глобальный x, было бы 101. Если локальный - 11.
        self.assert_evaluates(code, 11.0)


if __name__ == '__main__':
    unittest.main()