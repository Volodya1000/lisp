"""
Таблица символов с поддержкой вложенных областей видимости
"""
from dataclasses import dataclass
from typing import Dict, Optional, Any
from .ast_nodes import LambdaNode


@dataclass
class SymbolInfo:
    """Информация о символе в таблице"""
    name: str
    is_function: bool
    is_special_form: bool = False
    is_primitive: bool = False
    value: Any = None
    env_level: int = 0  # Уровень вложенности (для замыканий)


class Environment:
    """Окружение (цепочка областей видимости)"""

    # Примитивы, которые не могут быть переопределены
    PRIMITIVES = {
        'cons', 'car', 'cdr', 'atom', 'eq',
        '+', '-', '*', '/',
        'print', 'list', '='
    }

    # Специальные формы (не функции)
    SPECIAL_FORMS = {
        'quote', 'lambda', 'cond', 'setq'
    }

    def __init__(self, parent: Optional['Environment'] = None):
        self.parent = parent
        self.symbols: Dict[str, SymbolInfo] = {}
        self.level = parent.level + 1 if parent else 0

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
            env_level=self.level
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
        # Создаем в глобальном окружении
        if self.parent is None:
            return self.define(name)
        return self.get_global().define(name)

    def get_global(self) -> 'Environment':
        """Получить глобальное окружение"""
        if self.parent is None:
            return self
        return self.parent.get_global()

    def is_closed_over(self, name: str) -> bool:
        """Является ли символ захваченным замыканием?"""
        info = self.resolve(name)
        return info and info.env_level < self.level

    def __repr__(self):
        return f"Env(level={self.level}, symbols={list(self.symbols.keys())})"