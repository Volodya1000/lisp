from typing import Optional, Dict, Iterator
from contextlib import contextmanager
from semantic.symbol_info import SymbolInfo


class Scope:
    def __init__(self, parent: Optional["Scope"] = None):
        self.parent = parent
        self._symbols: Dict[str, SymbolInfo] = {}

    def define(self, name: str, info: SymbolInfo) -> None:
        """Добавляет символ в текущий scope без проверок."""
        self._symbols[name] = info

    def lookup(self, name: str) -> Optional[SymbolInfo]:
        """Ищет символ в текущем и всех родительских scope."""
        cur = self
        while cur is not None:
            if name in cur._symbols:
                return cur._symbols[name]
            cur = cur.parent
        return None

    def exists_in_current(self, name: str) -> bool:
        """Проверяет наличие символа только в текущем scope."""
        return name in self._symbols

    @contextmanager
    def nested_scope(self) -> Iterator["Scope"]:
        """Безопасный контекстный менеджер для создания вложенного scope."""
        new_scope = Scope(parent=self)
        yield new_scope