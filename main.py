"""
Точка входа в компилятор Lisp -> WASM
"""
import sys
from antlr4 import FileStream, CommonTokenStream
from gen.lispLexer import lispLexer
from gen.lispParser import lispParser
from semantic.semantic_analyzer import SemanticAnalyzer
from semantic.wasm_generator import WasmGenerator


def compile_file(filename: str) -> str:
    """Полный пайплайн компиляции"""
    # 1. Лексический анализ
    input_stream = FileStream(filename, encoding='utf-8')
    lexer = lispLexer(input_stream)
    tokens = CommonTokenStream(lexer)

    # 2. Синтаксический анализ
    parser = lispParser(tokens)
    tree = parser.program()

    # 3. Семантический анализ
    analyzer = SemanticAnalyzer()
    ast = analyzer.visit(tree)

    # 4. Генерация WASM
    generator = WasmGenerator()
    wat_code = generator.generate(ast)

    return wat_code


if __name__ == '__main__':
    if len(sys.argv) != 2:
        print(f"Usage: python {sys.argv[0]} <file.lisp>")
        sys.exit(1)

    try:
        wat = compile_file(sys.argv[1])
        print(wat)

        # Сохраняем в файл
        with open('output.wat', 'w', encoding='utf-8') as f:
            f.write(wat)
        print("\nСохранено в output.wat")
    except Exception as e:
        print(f"Ошибка компиляции: {e}", file=sys.stderr)
        sys.exit(1)