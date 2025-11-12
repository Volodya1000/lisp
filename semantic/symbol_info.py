from dataclasses import dataclass
from enum import Enum
from typing import Optional


class SymbolKind(Enum):
    VARIABLE = "var"
    FUNCTION = "function"
    BUILTIN = "builtin"


@dataclass(frozen=True)
class SymbolInfo:
    name: str
    kind: SymbolKind
    param_count: Optional[int] = None