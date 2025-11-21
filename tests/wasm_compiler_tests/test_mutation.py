import unittest
import antlr4
from gen.lispLexer import lispLexer
from gen.lispParser import lispParser
from semantic.semantic_analyzer import SemanticAnalyzer
from compiler.wasm_compiler import WasmCompiler


class TestWasmMutation(unittest.TestCase):
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

    def test_setq_simple(self):
        """Проверка изменения аргумента"""
        code = """
        (defun modify (x) 
            (setq x 100) 
            x)
        (modify 1)
        """
        wat = self.compile_code(code)

        # Мы ожидаем:
        # 1. f64.const 100
        # 2. local.tee 0 (присвоили и вернули)
        # 3. drop (так как setq не последнее выражение)
        # 4. local.get 0 (вернули x)
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
        wat = self.compile_code(code)
        self.assertIn("f64.add", wat)
        self.assertIn("local.tee", wat)

    def test_multi_statement_body(self):
        """Проверка того, что несколько выражений компилируются с drop"""
        code = """
        (defun useless-math (x)
            (+ x 1)     ; результат дропнется
            (* x 2)     ; результат дропнется
            x)          ; это вернется
        (useless-math 5)
        """
        wat = self.compile_code(code)

        # Должно быть 2 drop'а (для + и для *)
        self.assertIn("drop", wat)
        # Можно проверить количество вхождений, если хочется строгости
        self.assertEqual(wat.count("drop"), 2)

    def test_logic_not(self):
        # (not 0) -> 1, (not 123) -> 0
        wat = self.compile_code("(not 0)")
        self.assertIn("f64.eq", wat)  # not реализуется как (x == 0)


if __name__ == '__main__':
    unittest.main()