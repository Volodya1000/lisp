import logging
import sys
import os
import argparse
import antlr4
import wasmtime
import struct
import ctypes

# Указываем путь к корню проекта
ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
sys.path.insert(0, ROOT)

from gen.lispLexer import lispLexer
from gen.lispParser import lispParser
from semantic.semantic_analyzer import SemanticAnalyzer
from compiler.wasm_compiler import WasmCompiler

logging.basicConfig(level=logging.ERROR)
logging.getLogger().setLevel(logging.ERROR)
logging.getLogger("compiler").setLevel(logging.ERROR)
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
        # 1. Функция вывода числа (print)
        # ИЗМЕНЕНИЕ: Убран принудительный перевод строки (+ '\n')
        # Теперь (print 1) (print 2) выведет "12", а (print 1) (princ " ") (print 2) выведет "1 2"
        def print_number_impl(val):
            text = str(int(val)) if val.is_integer() else str(val)
            sys.stdout.write(text)
            sys.stdout.flush()

        # 2. Функция вывода строки (princ)
        def princ_impl(caller, ptr):
            mem = caller.get("memory")
            if mem is None: return

            if hasattr(mem, "data_ptr"):
                data_ptr = mem.data_ptr(caller)
                data_len = mem.data_len(caller)
                raw_memory = ctypes.string_at(data_ptr, data_len)
            else:
                return

            offset = int(ptr)
            if offset == 0: return

            # Читаем длину и байты строки
            length = struct.unpack('<I', raw_memory[offset:offset + 4])[0]
            str_bytes = raw_memory[offset + 4: offset + 4 + length]

            text = str_bytes.decode('utf-8', errors='replace')
            sys.stdout.write(text)
            sys.stdout.flush()

        # 3. Функция ВВОДА (read)
        def read_num_impl():
            try:
                sys.stdout.write(" > ")
                sys.stdout.flush()
                text = sys.stdin.readline()
                return float(text.strip())
            except ValueError:
                sys.stderr.write("[Runtime] Error: invalid number, defaulting to 0.0\n")
                return 0.0

        # Регистрируем функции

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
            # Отладка: сохраняем WAT файл
            with open("debug.wat", "w", encoding="utf-8") as f:
                f.write(wat_code)

            module = wasmtime.Module(self.engine, wat_code)
            instance = self.linker.instantiate(self.store, module)

            exports = instance.exports(self.store)
            main_func = exports["main"]

            # Запуск main
            result = main_func(self.store)

            # Вывод результата выполнения main (если нужно)
            if result != 0.0:
                print(f"\n[Finished] Result: {result}")
            else:
                print("\n[Finished]")

        except wasmtime.WasmtimeError as e:
            sys.stderr.write(f"[RUNTIME ERROR]: {e}\n")
        except Exception as e:
            sys.stderr.write(f"[ERROR]: {e}\n")


def main():
    parser = argparse.ArgumentParser(description="Run Lisp program via WASM compiler")
    parser.add_argument("file", help="Path to .lisp file")
    args = parser.parse_args()

    if not os.path.exists(args.file):
        print(f"Error: File '{args.file}' not found.")
        sys.exit(1)

    with open(args.file, 'r', encoding='utf-8') as f:
        code = f.read()

    print(f"--- Executing {args.file} ---")

    # Parsing
    try:
        input_stream = antlr4.InputStream(code)
        lexer = lispLexer(input_stream)
        stream = antlr4.CommonTokenStream(lexer)
        parser = lispParser(stream)
        tree = parser.program()
        if parser.getNumberOfSyntaxErrors() > 0:
            print("Parsing failed.")
            sys.exit(1)
    except Exception as e:
        print(f"Parsing Error: {e}")
        sys.exit(1)

    # Semantic Analysis
    try:
        analyzer = SemanticAnalyzer()
        ast = analyzer.visit(tree)
    except Exception as e:
        print(f"Semantic Error: {e}")
        sys.exit(1)

    # Compilation
    try:
        compiler = WasmCompiler()
        wat = compiler.compile(ast)
    except Exception as e:
        print(f"Compilation Error: {e}")
        sys.exit(1)

    # Execution
    runner = WasmRunner()
    runner.run(wat)


if __name__ == "__main__":
    main()