import sys
from antlr4 import *
from gen.lisp_grammerLexer import lisp_grammerLexer
from gen.lisp_grammerParser import lisp_grammerParser


def main(argv):
    # Читаем входной файл, переданный как аргумент командной строки
    input_stream = FileStream(argv[1], encoding='utf-8')

    # Создаем лексер и парсер
    lexer = lisp_grammerLexer(input_stream)
    stream = CommonTokenStream(lexer)
    parser = lisp_grammerParser(stream)

    # Запускаем парсер с начальным правилом 'program'
    tree = parser.program()

    # Выводим полученное дерево в текстовом виде
    print(tree.toStringTree(recog=parser))


if __name__ == '__main__':
    main(sys.argv)