import unittest
import struct
import wasmtime
import ctypes  # Добавлен импорт ctypes
from tests.wasm_compiler_tests.base_wasm_test import WasmCompilerTestCase


class TestStrings(WasmCompilerTestCase):

    def setUp(self):
        super().setUp()
        self.output_buffer = []

        def princ_impl(caller: wasmtime.Caller, ptr: float):
            extern = caller.get("memory")
            if extern is None:
                raise RuntimeError("Memory not exported")

            memory = extern
            # Если это обертка Extern (в некоторых версиях)
            if hasattr(extern, "memory") and not isinstance(extern, wasmtime.Memory):
                memory = extern.memory(caller)

            if ptr == 0.0:
                return

            offset = int(ptr)

            # Универсальное чтение памяти
            if hasattr(memory, "data"):
                data = memory.data(caller)
            elif hasattr(memory, "data_ptr"):
                data_ptr = memory.data_ptr(caller)
                data_len = memory.data_len(caller)
                # Читаем всю память как байты (неэффективно, но работает для тестов)
                data = ctypes.string_at(data_ptr, data_len)
            else:
                # Fallback: если memory это Extern, попробуем еще раз получить свойство
                # В совсем старых версиях caller.get возвращал Extern, у которого есть свойство memory
                try:
                    mem_obj = memory.memory
                    if callable(mem_obj):  # Если это метод
                        mem_obj = mem_obj(caller)

                    if hasattr(mem_obj, "data"):
                        data = mem_obj.data(caller)
                    else:
                        raise AttributeError
                except:
                    raise RuntimeError(f"Cannot access memory data on object: {memory}")

            # Читаем длину
            len_bytes = data[offset: offset + 4]
            length = struct.unpack('<I', len_bytes)[0]

            # Читаем строку
            str_bytes = data[offset + 4: offset + 4 + length]
            text = bytes(str_bytes).decode('utf-8')

            self.output_buffer.append(text)

        princ_type = wasmtime.FuncType([wasmtime.ValType.f64()], [])
        princ_func = wasmtime.Func(self.store, princ_type, princ_impl, access_caller=True)

        self.linker.define(self.store, "env", "princ", princ_func)

    def test_string_literal_allocation(self):
        wat = self.compile_to_wat('"hi"')
        ptr = self.run_wasm(wat)
        self.assertGreaterEqual(ptr, 8.0)

    def test_princ_hello(self):
        code = '(princ "Hello World")'
        self.assert_evaluates(code, 0.0)
        self.assertEqual(self.output_buffer, ["Hello World"])

    def test_multiple_strings(self):
        code = """
        (defun main-test ()
            (princ "First")
            (princ "Second"))
        (main-test)
        """
        self.assert_evaluates(code, 0.0)
        self.assertEqual(self.output_buffer, ["First", "Second"])

    def test_string_as_variable(self):
        code = """
        (setq msg "Dynamic Lisp")
        (princ msg)
        """
        self.assert_evaluates(code, 0.0)
        self.assertEqual(self.output_buffer, ["Dynamic Lisp"])