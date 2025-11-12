from dataclasses import dataclass
from typing import Optional, Any


@dataclass
class SymbolInfo:
    name: str
    kind: str  # 'var' or 'function'
    param_count: Optional[int] = None  # для функций (если известно), None = неизвестно
    extra: Any = None
