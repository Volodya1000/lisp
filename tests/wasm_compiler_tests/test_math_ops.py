import unittest
import antlr4
from gen.lispLexer import lispLexer
from gen.lispParser import lispParser
from semantic.semantic_analyzer import SemanticAnalyzer
from compiler.wasm_compiler import WasmCompiler


class TestWasmMath(unittest.TestCase):
    def compile_code(self, lisp_code: str) -> str:
        # 1. Lexer & Parser
        input_stream = antlr4.InputStream(lisp_code)
        lexer = lispLexer(input_stream)
        stream = antlr4.CommonTokenStream(lexer)
        parser = lispParser(stream)
        tree = parser.program()

        # 2. Semantic Analysis (AST)
        analyzer = SemanticAnalyzer()
        ast = analyzer.visit(tree)

        # 3. Compilation (WAT)
        compiler = WasmCompiler()
        return compiler.compile(ast)

    def test_simple_number(self):
        wat = self.compile_code("42")
        # Проверяем, что число есть в выводе
        self.assertIn("f64.const 42.0", wat)
        self.assertIn("(export \"main\")", wat)

    def test_addition(self):
        wat = self.compile_code("(+ 10 20)")
        # Ожидаем порядок: 10, 20, add
        expected_snippet = """f64.const 10.0
    f64.const 20.0
    f64.add"""
        self.assertIn(expected_snippet, wat)

    def test_nested_math(self):
        # (* (+ 1 2) 3) -> (1 + 2) * 3
        wat = self.compile_code("(* (+ 1 2) 3)")

        # Порядок выполнения в стековой машине:
        # 1. (+ 1 2) вычисляется первым -> кладет 3 на стек
        # 2. 3 кладется на стек
        # 3. * умножает

        expected_snippet = """f64.const 1.0
    f64.const 2.0
    f64.add
    f64.const 3.0
    f64.mul"""
        self.assertIn(expected_snippet, wat)

    def test_division(self):
        wat = self.compile_code("(/ 10 2)")
        self.assertIn("f64.div", wat)


if __name__ == '__main__':
    unittest.main()