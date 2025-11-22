import unittest
import antlr4
import wasmtime
from typing import Any, List, Tuple

# Импорты вашего проекта
from gen.lispLexer import lispLexer
from gen.lispParser import lispParser
from semantic.semantic_analyzer import SemanticAnalyzer
from compiler.wasm_compiler import WasmCompiler


class WasmCompilerTestCase(unittest.TestCase):
    """
    Базовый класс для всех тестов компилятора.
    Содержит методы для компиляции Lisp -> WAT и выполнения WASM.
    """

    def setUp(self):
        self.engine = wasmtime.Engine()
        self.store = wasmtime.Store(self.engine)
        self.linker = wasmtime.Linker(self.engine)
        self.linker.allow_shadowing = True
        self._setup_default_imports()

    def _setup_default_imports(self):
        """Определяет стандартные функции env.read_number, env.print_number и env.princ"""

        # --- Dummy input ---
        # Лямбда принимает 0 аргументов (по сигнатуре WASM)
        self.linker.define_func(
            "env", "read_number",
            wasmtime.FuncType([], [wasmtime.ValType.f64()]),
            lambda: 0.0
        )

        # --- Print number (console log) ---
        # Лямбда принимает 1 аргумент (число)
        self.linker.define_func(
            "env", "print_number",
            wasmtime.FuncType([wasmtime.ValType.f64()], []),
            lambda x: None  # print(f"[WASM Log]: {x}")
        )

        # --- Princ (String output) ---
        # Заглушка: принимает 1 аргумент (указатель)
        self.linker.define_func(
            "env", "princ",
            wasmtime.FuncType([wasmtime.ValType.f64()], []),
            lambda x: None
        )

    def compile_to_wat(self, lisp_code: str) -> str:
        input_stream = antlr4.InputStream(lisp_code)
        lexer = lispLexer(input_stream)
        stream = antlr4.CommonTokenStream(lexer)
        parser = lispParser(stream)
        tree = parser.program()

        analyzer = SemanticAnalyzer()
        ast = analyzer.visit(tree)

        compiler = WasmCompiler()
        return compiler.compile(ast)

    def run_wasm(self, wat_code: str) -> float:
        try:
            module = wasmtime.Module(self.engine, wat_code)
        except wasmtime.WasmtimeError as e:
            self.fail(f"WASM Compilation Error: {e}")

        try:
            instance = self.linker.instantiate(self.store, module)
        except wasmtime.WasmtimeError as e:
            self.fail(f"WASM Instantiation Error: {e}")

        main_func = instance.exports(self.store)["main"]
        return main_func(self.store)

    def assert_evaluates(self, code: str, expected: float, msg: str = None):
        wat = self.compile_to_wat(code)
        result = self.run_wasm(wat)

        debug_msg = f"\nCode: {code}\nExpected: {expected}\nGot: {result}"
        if msg:
            debug_msg += f"\nMessage: {msg}"

        self.assertAlmostEqual(result, expected, places=5, msg=debug_msg)
        return wat

    def assert_compile_error(self, code: str):
        with self.assertRaises(Exception):
            self.compile_to_wat(code)