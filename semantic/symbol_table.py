from dataclasses import dataclass
from typing import Dict, Optional, Any, Set


@dataclass
class SymbolInfo:
    name: str
    is_function: bool
    is_special_form: bool = False
    is_primitive: bool = False
    value: Any = None
    env_level: int = 0
    # Для замыканий: индекс переменной внутри кадра окружения (0, 1, 2...)
    # Если None, значит это глобальная переменная (WASM Global)
    var_index: Optional[int] = None


class Environment:
    PRIMITIVES = {
        'cons', 'car', 'cdr', 'atom', 'eq',
        '+', '-', '*', '/',
        'print', 'list', '=',
        '<', '>', '<=', '>=', 'not',
        'length', 'str-concat',
        'princ'
    }

    SPECIAL_FORMS = {
        'quote', 'lambda', 'cond', 'setq', 'defun',
        'progn', 'and', 'or'
    }

    def __init__(self, parent: Optional['Environment'] = None):
        self.parent = parent
        self.symbols: Dict[str, SymbolInfo] = {}
        self.level = parent.level + 1 if parent else 0
        # Счётчик локальных переменных в текущем скоупе (для аллокации в Heap)
        self.current_var_index = 0

    def define(self, name: str, is_function: bool = False, value: Any = None) -> SymbolInfo:
        if name in self.PRIMITIVES:
            is_primitive = True
            is_special = name in self.SPECIAL_FORMS
        else:
            is_primitive = False
            is_special = False

        # Если это не глобальный уровень (level > 0) и не функция/спецформа,
        # назначаем индекс для хранения в Heap-кадре
        idx = None
        if self.level > 0 and not is_function and not is_special and not is_primitive:
            idx = self.current_var_index
            self.current_var_index += 1

        info = SymbolInfo(
            name=name,
            is_function=is_function,
            is_special_form=is_special,
            is_primitive=is_primitive,
            value=value,
            env_level=self.level,
            var_index=idx
        )
        self.symbols[name] = info
        return info

    def resolve(self, name: str) -> Optional[SymbolInfo]:
        if name in self.symbols:
            return self.symbols[name]
        if self.parent:
            return self.parent.resolve(name)
        return None