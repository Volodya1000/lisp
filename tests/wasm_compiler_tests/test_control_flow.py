import unittest
import antlr4
from gen.lispLexer import lispLexer
from gen.lispParser import lispParser
from semantic.semantic_analyzer import SemanticAnalyzer
from compiler.wasm_compiler import WasmCompiler


class TestWasmControlFlow(unittest.TestCase):
    def compile_code(self, lisp_code: str) -> str:
        input_stream = antlr4.InputStream(lisp_code)
        lexer = lispLexer(input_stream)
        stream = antlr4.CommonTokenStream(lexer)
        parser = lispParser(stream)
        tree = parser.program()
        analyzer = SemanticAnalyzer()
        ast = analyzer.visit(tree)
        compiler = WasmCompiler()
        return compiler.compile(ast)

    def test_comparisons(self):
        # 10 > 5 -> 1.0, 10 < 5 -> 0.0
        wat = self.compile_code("(> 10 5)")
        self.assertIn("f64.gt", wat)
        self.assertIn("f64.convert_i32_s", wat)  # Проверка конвертации типа

    def test_simple_cond(self):
        # (defun check (x) (cond ((> x 10) 1) (t 0)))
        code = """
        (defun check (x)
            (cond 
                ((> x 10) 100) 
                (t 200)))
        (check 5)
        """
        wat = self.compile_code(code)

        # Проверяем структуру if
        self.assertIn("(if (result f64)", wat)
        self.assertIn("(then", wat)
        self.assertIn("(else", wat)

        # Проверяем, что есть проверка условия
        self.assertIn("f64.ne", wat)  # Проверка на boolean

    def test_factorial(self):
        """Тест компиляции рекурсивного факториала"""
        code = """
        (defun fact (n)
            (cond 
                ((= n 1) 1)
                (t (* n (fact (- n 1))))))
        (fact 5)
        """
        wat = self.compile_code(code)

        # Проверяем ключевые элементы
        self.assertIn("func $fact", wat)
        self.assertIn("f64.eq", wat)  # Проверка n == 1
        self.assertIn("call $fact", wat)  # Рекурсивный вызов
        self.assertIn("f64.sub", wat)  # n - 1
        self.assertIn("f64.mul", wat)  # * n ...


if __name__ == '__main__':
    unittest.main()