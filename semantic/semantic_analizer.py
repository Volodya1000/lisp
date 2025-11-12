from typing import List

from semantic.builtin_functions import BUILTIN_FUNCTIONS
from semantic.errors import SemanticError
from semantic.symbol_info import SymbolInfo
from semantic.scope import Scope
from semantic.semantic_visitor import SemanticVisitor


class SemanticAnalyzer:
    def __init__(self):
        self.global_scope = Scope(None)
        # Инициализируем global_scope здесь, один раз
        for name, meta in BUILTIN_FUNCTIONS.items():
            self.global_scope.symbols[name] = SymbolInfo(name=name, kind="function", param_count=meta["param_count"])

    def analyze(self, parse_tree) -> List[SemanticError]:
        # Создаем *новый* visitor *каждый раз*,
        # но передаем ему *тот же* global_scope
        visitor = SemanticVisitor(global_scope=self.global_scope)

        visit = getattr(visitor, "visit", None)
        if visit is None:
            raise RuntimeError("Visitor interface not available. Generate parser with -visitor option in ANTLR.")

        visitor.visit(parse_tree)
        return visitor.errors
