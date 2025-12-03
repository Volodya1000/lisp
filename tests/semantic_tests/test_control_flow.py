from semantic.ast_nodes import CondNode
from semantic.diagnostics import InvalidSyntaxError
from tests.semantic_tests.base_semantic_test import BaseSemanticTest

class TestControlFlow(BaseSemanticTest):
    def test_cond_simple(self):
        nodes, _ = self.parse_and_analyze("(cond (t 42))")
        assert isinstance(nodes[0], CondNode)
        assert len(nodes[0].clauses) == 1

    def test_cond_multiple_exprs_in_clause(self):
        """В clause может быть несколько выражений"""
        nodes, _ = self.parse_and_analyze("(cond (t (setq x 1) x))")
        clause_body = nodes[0].clauses[0][1]
        assert len(clause_body) == 2

    def test_cond_invalid_clause(self):
        """Ошибка: Clause должен быть списком"""
        _, analyzer = self.parse_and_analyze("(cond 1)")
        assert isinstance(analyzer.collector.errors[0], InvalidSyntaxError)