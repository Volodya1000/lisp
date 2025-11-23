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


class WasmRunner:
    def __init__(self):
        self.engine = wasmtime.Engine()
        self.store = wasmtime.Store(self.engine)
        self.linker = wasmtime.Linker(self.engine)
        self.linker.allow_shadowing = True
        self._setup_imports()

    def _setup_imports(self):
        # 1. Функция вывода числа (print)
        # ИСПРАВЛЕНО: Убран аргумент 'caller', так как access_caller=False по умолчанию
        def print_number_impl(val):
            if val.is_integer():
                print(int(val))
            else:
                print(val)

        # 2. Функция вывода строки (princ)
        # Здесь нужен caller для доступа к памяти, поэтому access_caller=True остается
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

            length = struct.unpack('<I', raw_memory[offset:offset + 4])[0]
            str_bytes = raw_memory[offset + 4: offset + 4 + length]
            text = str_bytes.decode('utf-8', errors='replace')
            print(text, end='', flush=True)

        # 3. Функция ВВОДА (read)
        # ИСПРАВЛЕНО: Убран аргумент 'caller', он здесь не нужен
        def read_num_impl():
            try:
                text = input(" > ")
                return float(text)
            except ValueError:
                print("[Runtime] Ошибка: введено не число, использую 0.0")
                return 0.0

        # Регистрируем функции

        # print_number: принимает f64, ничего не возвращает
        self.linker.define_func(
            "env", "print_number",
            wasmtime.FuncType([wasmtime.ValType.f64()], []),
            print_number_impl
        )

        # princ: принимает f64 (ptr), нужен доступ к памяти (access_caller=True)
        self.linker.define_func(
            "env", "princ",
            wasmtime.FuncType([wasmtime.ValType.f64()], []),
            princ_impl,
            access_caller=True
        )

        # read_num: 0 аргументов, возвращает f64
        self.linker.define_func(
            "env", "read_num",
            wasmtime.FuncType([], [wasmtime.ValType.f64()]),
            read_num_impl
        )

    def run(self, wat_code: str):
        try:
            module = wasmtime.Module(self.engine, wat_code)
            instance = self.linker.instantiate(self.store, module)

            # В новой версии компилятора код инициализации может быть разбросан,
            # но точка входа всегда main
            main_func = instance.exports(self.store)["main"]

            result = main_func(self.store)

            # Если результат не 0 (princ обычно возвращает 0), выведем его
            if result != 0.0:
                print(f"\n[Finished] Result: {result}")
            else:
                print("\n[Finished]")

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