from abc import ABC, abstractmethod
from typing import Any, TYPE_CHECKING

# Используем TYPE_CHECKING. Во время выполнения этот блок игнорируется,
# поэтому циклического импорта не происходит.
if TYPE_CHECKING:
    from .ast_nodes import (
        SymbolNode, NumberNode, StringNode, NilNode, TrueNode,
        QuoteNode, ListNode, LambdaNode, CallNode, PrimCallNode,
        SetqNode, CondNode, DefunNode, PrognNode, LogicNode
    )


class ASTVisitor(ABC):
    """Абстрактный интерфейс для компиляторов и других обработчиков"""

    # Используем строковые аннотации ('SymbolNode'), чтобы Python не искал класс
    # во время определения методов (когда класс еще не импортирован).

    @abstractmethod
    def visit_symbol(self, node: 'SymbolNode') -> Any: pass

    @abstractmethod
    def visit_number(self, node: 'NumberNode') -> Any: pass

    @abstractmethod
    def visit_string(self, node: 'StringNode') -> Any: pass

    @abstractmethod
    def visit_nil(self, node: 'NilNode') -> Any: pass

    @abstractmethod
    def visit_true(self, node: 'TrueNode') -> Any: pass

    @abstractmethod
    def visit_quote(self, node: 'QuoteNode') -> Any: pass

    @abstractmethod
    def visit_list(self, node: 'ListNode') -> Any: pass

    @abstractmethod
    def visit_lambda(self, node: 'LambdaNode') -> Any: pass

    @abstractmethod
    def visit_call(self, node: 'CallNode') -> Any: pass

    @abstractmethod
    def visit_prim_call(self, node: 'PrimCallNode') -> Any: pass

    @abstractmethod
    def visit_setq(self, node: 'SetqNode') -> Any: pass

    @abstractmethod
    def visit_cond(self, node: 'CondNode') -> Any: pass

    @abstractmethod
    def visit_defun(self, node: 'DefunNode') -> Any: pass

    @abstractmethod
    def visit_progn(self, node: 'PrognNode') -> Any: pass

    @abstractmethod
    def visit_logic(self, node: 'LogicNode') -> Any: pass


class ASTNode(ABC):
    """Базовый класс для всех AST узлов"""

    # ASTNode находится здесь, так как он тесно связан с Visitor
    # и является абстракцией, а не реализацией.

    @abstractmethod
    def accept(self, visitor: ASTVisitor) -> Any:
        pass