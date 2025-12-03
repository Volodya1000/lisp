import unittest
import struct
import ctypes
import antlr4
import wasmtime
from typing import List
from gen.lispLexer import lispLexer
from gen.lispParser import lispParser
from semantic.semantic_analyzer import SemanticAnalyzer
from compiler.wasm_compiler import WasmCompiler


class WasmCompilerTestCase(unittest.TestCase):
    def setUp(self):
        self.engine = wasmtime.Engine()
        self.store = wasmtime.Store(self.engine)
        self.linker = wasmtime.Linker(self.engine)
        self.linker.allow_shadowing = True

        self.output_buffer: List[str] = []
        self._setup_imports()

    def _setup_imports(self):
        self.linker.define_func(
            "env", "read_num",
            wasmtime.FuncType([], [wasmtime.ValType.f64()]),
            lambda: 0.0
        )

        self.linker.define_func(
            "env", "print_number",
            wasmtime.FuncType([wasmtime.ValType.f64()], []),
            lambda x: None  # print(f"[LOG]: {x}")
        )

        princ_type = wasmtime.FuncType([wasmtime.ValType.f64()], [])
        princ_func = wasmtime.Func(self.store, princ_type, self._princ_impl, access_caller=True)
        self.linker.define(self.store, "env", "princ", princ_func)

    def _princ_impl(self, caller: wasmtime.Caller, ptr: float):
        """Читает строку из памяти WASM по указателю ptr и сохраняет в буфер"""
        if ptr == 0.0:
            return

        extern = caller.get("memory")
        if extern is None:
            raise RuntimeError("Memory not exported")

        memory = extern
        if hasattr(extern, "memory") and not isinstance(extern, wasmtime.Memory):
            memory = extern.memory(caller)

        offset = int(ptr)

        data_ptr = memory.data_ptr(caller)
        data_len = memory.data_len(caller)
        data = ctypes.string_at(data_ptr, data_len)

        # Формат строки в куче: [Length: 4 bytes] + [Bytes...]
        try:
            len_bytes = data[offset: offset + 4]
            length = struct.unpack('<I', len_bytes)[0]
            str_bytes = data[offset + 4: offset + 4 + length]
            text = bytes(str_bytes).decode('utf-8')
            self.output_buffer.append(text)
        except Exception as e:
            print(f"Error reading string from WASM memory at {offset}: {e}")

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
            instance = self.linker.instantiate(self.store, module)
            main_func = instance.exports(self.store)["main"]
            return main_func(self.store)
        except wasmtime.WasmtimeError as e:
            self.fail(f"WASM Runtime Error: {e}")
        except Exception as e:
            self.fail(f"Compilation/Execution Error: {e}")

    def assert_evaluates(self, code: str, expected: float, msg: str = None):
        wat = self.compile_to_wat(code)
        result = self.run_wasm(wat)

        debug_msg = f"\nCode: {code}\nExpected: {expected}\nGot: {result}"
        if msg: debug_msg += f"\nMessage: {msg}"

        self.assertAlmostEqual(result, expected, places=5, msg=debug_msg)
        return wat