import sys
import os
import argparse
import antlr4
import wasmtime
import struct
import ctypes

# Добавляем корневую директорию в путь, чтобы импорты работали
ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
sys.path.insert(0, ROOT)


from gen.lispLexer import lispLexer
from gen.lispParser import lispParser
from semantic.semantic_analyzer import SemanticAnalyzer
from compiler.wasm_compiler import WasmCompiler


class WasmRunner:
    def __init__(self):
        self.engine = wasmtime.Engine()
        self.store = wasmtime.Store(self.engine)
        self.linker = wasmtime.Linker(self.engine)
        self.linker.allow_shadowing = True
        self._setup_imports()

    def _setup_imports(self):
        # Реализация функций, которые Lisp может вызывать (print, princ)

        def print_number_impl(caller, val):
            print(f"[LISP OUTPUT]: {val}")

        def princ_impl(caller, ptr):
            # Получаем доступ к памяти
            mem = caller.get("memory")
            if mem is None: return

            # Поддержка разных версий wasmtime
            if hasattr(mem, "data_ptr"):
                data_ptr = mem.data_ptr(caller)
                data_len = mem.data_len(caller)
                raw_memory = ctypes.string_at(data_ptr, data_len)
            else:
                # Fallback (может не работать в новых версиях)
                print("[WARN] Cannot access memory for princ")
                return

            offset = int(ptr)
            if offset == 0: return

            # Читаем длину строки (первые 4 байта)
            length = struct.unpack('<I', raw_memory[offset:offset + 4])[0]
            # Читаем байты строки
            str_bytes = raw_memory[offset + 4: offset + 4 + length]
            text = str_bytes.decode('utf-8', errors='replace')

            print(text, end='')  # princ не ставит перенос строки

        # Регистрируем функции в "env"
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

    def run(self, wat_code: str):
        try:
            module = wasmtime.Module(self.engine, wat_code)
            instance = self.linker.instantiate(self.store, module)

            main_func = instance.exports(self.store)["main"]
            result = main_func(self.store)

            print(f"\n[PROGRAM FINISHED]")
            print(f"Result: {result}")

        except wasmtime.WasmtimeError as e:
            print(f"[RUNTIME ERROR]: {e}")
        except Exception as e:
            print(f"[ERROR]: {e}")


def main():
    parser = argparse.ArgumentParser(description="Run Lisp program via WASM compiler")
    parser.add_argument("file", help="Path to .lisp file")
    args = parser.parse_args()

    if not os.path.exists(args.file):
        print(f"Error: File '{args.file}' not found.")
        sys.exit(1)

    with open(args.file, 'r', encoding='utf-8') as f:
        code = f.read()

    print(f"--- Running {args.file} ---")

    # 1. Parsing
    try:
        input_stream = antlr4.InputStream(code)
        lexer = lispLexer(input_stream)
        stream = antlr4.CommonTokenStream(lexer)
        parser = lispParser(stream)

        # Отключаем стандартный вывод ошибок ANTLR в консоль, если нужно
        # parser.removeErrorListeners()

        tree = parser.program()

        if parser.getNumberOfSyntaxErrors() > 0:
            print("Parsing failed with syntax errors.")
            sys.exit(1)

    except Exception as e:
        print(f"Parsing Error: {e}")
        sys.exit(1)

    # 2. Semantic Analysis
    try:
        analyzer = SemanticAnalyzer()
        ast = analyzer.visit(tree)
        print("[OK] Semantic Analysis passed.")
    except Exception as e:
        print(f"\n[SEMANTIC ERROR]: {e}")
        sys.exit(1)

    # 3. Compilation
    try:
        compiler = WasmCompiler()
        wat = compiler.compile(ast)
        print("[OK] Compilation to WAT successful.")
    except Exception as e:
        print(f"\n[COMPILATION ERROR]: {e}")
        sys.exit(1)

    # 4. Execution
    print("-" * 30)
    runner = WasmRunner()
    runner.run(wat)
    print("-" * 30)


if __name__ == "__main__":
    main()