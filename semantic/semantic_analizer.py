from typing import List

from semantic import SemanticError
from semantic.builtin_functions import BUILTINS
from semantic.scope import Scope
from semantic.symbol_info import SymbolInfo, SymbolKind
from semantic.semantic_visitor import SemanticVisitor


class SemanticAnalyzer:
    def __init__(self):
        self.global_scope = self._initialize_global_scope()

    def _initialize_global_scope(self) -> Scope:
        """Создает глобальный scope с встроенными функциями."""
        scope = Scope()
        for info in BUILTINS.values():
            scope.define(info.name, info)
        return scope

    def analyze(self, parse_tree) -> List[SemanticError]:
        """Запускает семантический анализ и возвращает список ошибок."""
        visitor = SemanticVisitor(self.global_scope)
        visitor.visit(parse_tree)
        return visitor.errors