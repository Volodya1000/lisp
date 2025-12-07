import sys
import logging
import wasmtime
import struct
import ctypes

logging.basicConfig(
    level=logging.ERROR,
    format='%(levelname)s: %(message)s'
)
logging.getLogger("compiler.wasm_compiler").setLevel(logging.INFO)
logging.getLogger("compiler.wasm_context").setLevel(logging.INFO)


class WasmRunner:
    def __init__(self):
        self.engine = wasmtime.Engine()
        self.store = wasmtime.Store(self.engine)
        self.linker = wasmtime.Linker(self.engine)
        self.linker.allow_shadowing = True
        self._setup_imports()

    def _setup_imports(self):
        def print_number_impl(val: float):
            text = str(int(val)) if val.is_integer() else str(val)
            sys.stdout.write(text)
            sys.stdout.flush()

        def princ_impl(caller, ptr: float):
            mem = caller.get("memory")
            if not mem:
                return
            try:
                data_ptr = mem.data_ptr(caller)
                data_len = mem.data_len(caller)
                raw_memory = ctypes.string_at(data_ptr, data_len)

                offset = int(ptr)
                if offset == 0:
                    return

                length = struct.unpack('<I', raw_memory[offset:offset + 4])[0]
                str_bytes = raw_memory[offset + 4: offset + 4 + length]

                text = str_bytes.decode('utf-8', errors='replace')
                sys.stdout.write(text)
                sys.stdout.flush()
            except Exception:
                pass

        def read_num_impl():
            try:
                sys.stdout.write(" > ")
                sys.stdout.flush()
                return float(sys.stdin.readline().strip())
            except ValueError:
                sys.stderr.write("[Runtime] Error: invalid number, defaulting to 0.0\n")
                return 0.0

        self.linker.define_func(
            "env", "print_number",
            wasmtime.FuncType([wasmtime.ValType.f64()], []),
            print_number_impl
        )
        self.linker.define_func(
            "env", "princ",
            wasmtime.FuncType([wasmtime.ValType.f64()], []),
            princ_impl,
            access_caller=True
        )
        self.linker.define_func(
            "env", "read_num",
            wasmtime.FuncType([], [wasmtime.ValType.f64()]),
            read_num_impl
        )

    def run(self, wat_code: str):
        try:
            module = wasmtime.Module(self.engine, wat_code)
            instance = self.linker.instantiate(self.store, module)
            result = instance.exports(self.store)["main"](self.store)

            if result != 0.0:
                print(f"\n[Finished] Result: {result}")
            else:
                print("\n[Finished]")
        except wasmtime.WasmtimeError as e:
            sys.stderr.write(f"[RUNTIME ERROR]: {e}\n")
            sys.exit(1)