import sys
import os
import argparse
import antlr4

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
sys.path.insert(0, ROOT)


from gen.lispLexer import lispLexer
from gen.lispParser import lispParser
from semantic.semantic_analyzer import SemanticAnalyzer
from compiler.wasm_compiler import WasmCompiler


def main():
    parser = argparse.ArgumentParser(description="Compile Lisp file to WebAssembly Text (WAT)")
    parser.add_argument("file", help="Path to .lisp file")
    parser.add_argument("-o", "--output", help="Output .wat file path (optional)")
    args = parser.parse_args()

    if not os.path.exists(args.file):
        print(f"Error: File '{args.file}' not found.")
        sys.exit(1)

    with open(args.file, 'r', encoding='utf-8') as f:
        code = f.read()

    # 1. Parse & Analyze
    try:
        input_stream = antlr4.InputStream(code)
        lexer = lispLexer(input_stream)
        stream = antlr4.CommonTokenStream(lexer)
        parser = lispParser(stream)
        tree = parser.program()

        if parser.getNumberOfSyntaxErrors() > 0:
            print("Parsing failed.", file=sys.stderr)
            sys.exit(1)

        analyzer = SemanticAnalyzer()
        ast = analyzer.visit(tree)

    except Exception as e:
        print(f"Error during analysis: {e}", file=sys.stderr)
        sys.exit(1)

    # 2. Compile
    try:
        compiler = WasmCompiler()
        wat = compiler.compile(ast)
    except Exception as e:
        print(f"Error during compilation: {e}", file=sys.stderr)
        sys.exit(1)

    # 3. Output
    if args.output:
        with open(args.output, 'w', encoding='utf-8') as f:
            f.write(wat)
        print(f"Successfully compiled to {args.output}")
    else:
        print(wat)


if __name__ == "__main__":
    main()