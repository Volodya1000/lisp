from semantic.ast_nodes import NumberNode, StringNode, NilNode, TrueNode, QuoteNode, ListNode
from semantic.diagnostics import ArityError
from tests.semantic_tests.base_semantic_test import BaseSemanticTest

class TestLiterals(BaseSemanticTest):
    def test_number_literal(self):
        nodes, _ = self.parse_and_analyze("42")
        assert isinstance(nodes[0], NumberNode)
        assert nodes[0].value == 42

    def test_string_literal(self):
        nodes, _ = self.parse_and_analyze('"hello"')
        assert isinstance(nodes[0], StringNode)
        assert nodes[0].value == "hello"

    def test_bool_literals(self):
        """Проверка t и nil"""
        nodes, _ = self.parse_and_analyze("t nil")
        assert isinstance(nodes[0], TrueNode)
        assert isinstance(nodes[1], NilNode)

    def test_empty_list_is_nil(self):
        """Пустой список () эквивалентен nil"""
        nodes, _ = self.parse_and_analyze("()")
        assert isinstance(nodes[0], NilNode)

    def test_quote_list(self):
        nodes, _ = self.parse_and_analyze("'(a b c)")
        assert isinstance(nodes[0], QuoteNode)
        assert isinstance(nodes[0].expr, ListNode)
        assert len(nodes[0].expr.elements) == 3

    def test_nested_quote(self):
        """Проверка вложенного quote '''a"""
        nodes, _ = self.parse_and_analyze("'''a")
        assert isinstance(nodes[0], QuoteNode)
        assert isinstance(nodes[0].expr, QuoteNode)

    def test_quote_no_args_error(self):
        """Ошибка: (quote) без аргументов"""
        _, analyzer = self.parse_and_analyze("(quote)")
        assert analyzer.collector.has_errors()
        assert isinstance(analyzer.collector.errors[0], ArityError)