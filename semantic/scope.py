from typing import Optional, Dict

from semantic.errors import DuplicateDeclarationError
from semantic.utils import _pos_from_ctx
from semantic.symbol_info import SymbolInfo


class Scope:
    def __init__(self, parent: Optional["Scope"] = None):
        self.parent = parent
        self.symbols: Dict[str, SymbolInfo] = {}

    def define(self, name: str, info: SymbolInfo, ctx=None):
        if name in self.symbols:
            line, col = _pos_from_ctx(ctx)
            raise DuplicateDeclarationError(f"Duplicate declaration of '{name}' in the same scope", line, col)
        self.symbols[name] = info

    def lookup(self, name: str) -> Optional[SymbolInfo]:
        cur = self
        while cur is not None:
            if name in cur.symbols:
                return cur.symbols[name]
            cur = cur.parent
        return None

    def exists_in_current(self, name: str) -> bool:
        return name in self.symbols
