import unittest
import antlr4
from gen.lispLexer import lispLexer
from gen.lispParser import lispParser
from semantic.semantic_analyzer import SemanticAnalyzer
from compiler.wasm_compiler import WasmCompiler


class TestWasmFunctions(unittest.TestCase):
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

    def test_simple_defun(self):
        # Функция: (defun id (x) x)
        code = """
        (defun id (x) x)
        (id 42)
        """
        wat = self.compile_code(code)

        # Проверка 1: Функция объявлена с параметром и результатом
        self.assertIn("(func $id (param f64) (result f64)", wat)

        # Проверка 2: Внутри функции используется local.get 0 (для x)
        # local.get 0 берет первый параметр
        self.assertIn("local.get 0", wat)

        # Проверка 3: В main есть вызов
        self.assertIn("call $id", wat)

    def test_math_in_function(self):
        # Функция сложения трех чисел
        code = """
        (defun add3 (a b c) 
            (+ a (+ b c)))
        (add3 1 2 3)
        """
        wat = self.compile_code(code)

        self.assertIn("(func $add3 (param f64) (param f64) (param f64)", wat)

        # Проверяем порядок индексов (a=0, b=1, c=2)
        # Порядок в коде может зависеть от порядка вычисления аргументов +,
        # но local.get 0, local.get 1, local.get 2 должны присутствовать.
        self.assertIn("local.get 0", wat)
        self.assertIn("local.get 1", wat)
        self.assertIn("local.get 2", wat)

    def test_function_call_chain(self):
        # square вызывает multiply
        code = """
        (defun mul (a b) (* a b))
        (defun square (x) (mul x x))
        (square 10)
        """
        wat = self.compile_code(code)

        # Внутри square должен быть вызов mul
        # x имеет индекс 0
        # Код должен быть примерно: local.get 0, local.get 0, call $mul
        self.assertIn("call $mul", wat)
        self.assertIn("(func $square", wat)


if __name__ == '__main__':
    unittest.main()