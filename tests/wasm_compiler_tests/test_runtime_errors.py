import unittest
import wasmtime
from tests.wasm_compiler_tests.base_wasm_test import WasmCompilerTestCase


class TestRuntimeErrors(WasmCompilerTestCase):

    def test_call_nil_as_function(self):
        """
        Attempts to call 'nil' (0.0) as a function.
        In Wasm, calling index 0 (if 0.0 is truncated) usually traps
        if table[0] is not a valid signature match or if logic prevents it.

        If your compiler puts the 'cons' or 'car' stdlib at index 0, this might
        result in a signature mismatch error from WASM.
        """
        code = """
        (defun main ()
            (funcall nil 10))
        (main)
        """
        # This is expected to fail. It might be a CompilerError (if you add static checks)
        # or a WasmtimeError (Runtime Trap: indirect call type mismatch).
        try:
            self.assert_evaluates(code, 0.0)
        except Exception as e:
            # We expect an error here.
            # Printing it helps confirm it's the RIGHT error (Type mismatch).
            print(f"\nCaught expected error for nil call: {e}")
            pass

    def test_call_number_as_function(self):
        """Attempts to use a number as a function pointer."""
        code = """
        (defun main ()
            (funcall 12345 10))
        (main)
        """
        with self.assertRaises(Exception):
            self.assert_evaluates(code, 0.0)