from gen.lispParser import lispParser
from .ast_nodes import *

class AtomFactory:
    @staticmethod
    def create(ctx: lispParser.AtomContext) -> ASTNode:
        """
        Создает AST узел из контекста атома.
        Analyzer передается для рекурсивного вызова quote (редкий кейс внутри атома).
        """
        if ctx.NUMBER():
            return NumberNode(float(ctx.NUMBER().getText()))
        elif ctx.STRING():
            # Убираем кавычки
            return StringNode(ctx.STRING().getText()[1:-1])
        elif ctx.SYMBOL():
            name = ctx.SYMBOL().getText()
            # Попытка интерпретировать символ как число
            try:
                return NumberNode(float(name))
            except ValueError:
                pass

            if name == 'nil':
                return NilNode()
            elif name == 't':
                return TrueNode()
            else:
                return SymbolNode(name)
        elif ctx.NIL():
            return NilNode()
        elif ctx.TRUE():
            return TrueNode()
        elif ctx.QUOTE():
            # 'expr внутри атома делегируем билдеру через analyzer (или напрямую)
            # Чтобы избежать циклического импорта, используем quote_builder в анализаторе,
            # но здесь atom ctx.quote требует sexpr.
            # В оригинале: return QuoteNode(self._build_quoted_data(ctx.sexpr()))
            # Здесь мы вернем управление анализатору, так как QuoteBuilder требует парсинга
            from .quote_builder import QuoteBuilder
            return QuoteNode(QuoteBuilder.build(ctx.sexpr()))
        else:
            # Fallback
            return NilNode()