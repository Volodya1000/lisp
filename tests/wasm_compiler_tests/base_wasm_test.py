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
        # Инициализируем движок один раз на тест
        self.engine = wasmtime.Engine()
        self.store = wasmtime.Store(self.engine)
        self.linker = wasmtime.Linker(self.engine)

        # Настраиваем стандартные моки (input/output)
        self._setup_default_imports()

    def _setup_default_imports(self):
        """Определяет стандартные функции env.read_number и env.print_number"""
        self.linker.define_func(
            "env", "read_number",
            wasmtime.FuncType([], [wasmtime.ValType.f64()]),
            lambda caller: 0.0  # Заглушка ввода
        )

        self.linker.define_func(
            "env", "print_number",
            wasmtime.FuncType([wasmtime.ValType.f64()], []),
            lambda caller, x: print(f"[WASM Log]: {x}")
        )

    def compile_to_wat(self, lisp_code: str) -> str:
        """Преобразует Lisp код в WAT (текстовый WebAssembly)"""
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
        """Компилирует WAT в бинарный модуль и исполняет его"""
        try:
            module = wasmtime.Module(self.engine, wat_code)
        except wasmtime.WasmtimeError as e:
            self.fail(f"WASM Compilation Error: {e}")

        try:
            instance = self.linker.instantiate(self.store, module)
        except wasmtime.WasmtimeError:
            # Если импорты не нужны
            instance = wasmtime.Instance(self.store, module, [])

        main_func = instance.exports(self.store)["main"]
        return main_func(self.store)

    def assert_evaluates(self, code: str, expected: float, msg: str = None):
        """Основной метод проверки: Code -> Run -> Assert Result"""
        wat = self.compile_to_wat(code)
        result = self.run_wasm(wat)

        debug_msg = f"\nCode: {code}\nExpected: {expected}\nGot: {result}"
        if msg:
            debug_msg += f"\nMessage: {msg}"

        self.assertAlmostEqual(result, expected, places=5, msg=debug_msg)
        return wat  # Возвращаем WAT, если тест захочет проверить структуру кода

    def assert_compile_error(self, code: str):
        """Проверяет, что код вызывает ошибку компиляции (например, семантическую)"""
        with self.assertRaises(Exception):
            self.compile_to_wat(code)