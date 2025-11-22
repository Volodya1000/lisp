from dataclasses import dataclass
from typing import Dict, Optional, Any


@dataclass
class SymbolInfo:
    """Информация о символе в таблице"""
    name: str
    is_function: bool
    is_special_form: bool = False
    is_primitive: bool = False
    value: Any = None
    env_level: int = 0
    wasm_loc_idx: Optional[int] = None


class Environment:
    """Окружение (цепочка областей видимости)"""

    # Примитивы
    PRIMITIVES = {
        'cons', 'car', 'cdr', 'atom', 'eq',
        '+', '-', '*', '/',
        'print', 'list', '=',
        '<', '>', '<=', '>=', 'not',
        'length', 'str-concat',
        'princ' 
    }

    # Специальные формы
    SPECIAL_FORMS = {
        'quote', 'lambda', 'cond', 'setq', 'defun',
        'progn', 'and', 'or'
    }

    def __init__(self, parent: Optional['Environment'] = None):
        self.parent = parent
        self.symbols: Dict[str, SymbolInfo] = {}
        self.level = parent.level + 1 if parent else 0
        self.next_local_idx = 0

    def define(self, name: str, is_function: bool = False, value: Any = None) -> SymbolInfo:
        """Определить новый символ в текущем окружении"""
        if name in self.PRIMITIVES:
            is_primitive = True
            is_special = name in self.SPECIAL_FORMS
        else:
            is_primitive = False
            is_special = False

        info = SymbolInfo(
            name=name,
            is_function=is_function,
            is_special_form=is_special,
            is_primitive=is_primitive,
            value=value,
            env_level=self.level,
            wasm_loc_idx=None
        )
        self.symbols[name] = info
        return info

    def resolve(self, name: str) -> Optional[SymbolInfo]:
        """Найти символ в цепочке окружений"""
        if name in self.symbols:
            return self.symbols[name]
        if self.parent:
            return self.parent.resolve(name)
        return None

    def resolve_or_create(self, name: str) -> SymbolInfo:
        """Для глобальных переменных: создать, если не существует"""
        info = self.resolve(name)
        if info:
            return info
        if self.parent is None:
            return self.define(name)
        return self.get_global().define(name)

    def get_global(self) -> 'Environment':
        if self.parent is None:
            return self
        return self.parent.get_global()

    def define_wasm_local(self, name: str) -> int:
        info = self.define(name, is_function=False)
        info.wasm_loc_idx = self.next_local_idx
        self.next_local_idx += 1
        return info.wasm_loc_idx

    def __repr__(self):
        return f"Env(level={self.level}, symbols={list(self.symbols.keys())})"