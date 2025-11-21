"""
AST узлы, реализующие паттерн Visitor для компиляции
"""
from abc import ABC, abstractmethod
from typing import List, Any


class ASTVisitor(ABC):
    """Абстрактный интерфейс для компиляторов и других обработчиков"""

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


class ASTNode(ABC):
    """Базовый класс для всех AST узлов"""

    @abstractmethod
    def accept(self, visitor: ASTVisitor) -> Any:
        """Паттерн Visitor для компиляции"""
        pass


class SymbolNode(ASTNode):
    def __init__(self, name: str):
        self.name = name

    def accept(self, visitor: ASTVisitor) -> Any:
        return visitor.visit_symbol(self)

    def __repr__(self):
        return f"Symbol({self.name})"


class NumberNode(ASTNode):
    def __init__(self, value: float):
        self.value = value

    def accept(self, visitor: ASTVisitor) -> Any:
        return visitor.visit_number(self)

    def __repr__(self):
        return f"Number({self.value})"


class StringNode(ASTNode):
    def __init__(self, value: str):
        self.value = value

    def accept(self, visitor: ASTVisitor) -> Any:
        return visitor.visit_string(self)

    def __repr__(self):
        return f"String({self.value!r})"


class NilNode(ASTNode):
    def accept(self, visitor: ASTVisitor) -> Any:
        return visitor.visit_nil(self)

    def __repr__(self):
        return "Nil()"


class TrueNode(ASTNode):
    def accept(self, visitor: ASTVisitor) -> Any:
        return visitor.visit_true(self)

    def __repr__(self):
        return "True()"


class QuoteNode(ASTNode):
    def __init__(self, expr: ASTNode):
        self.expr = expr

    def accept(self, visitor: ASTVisitor) -> Any:
        return visitor.visit_quote(self)

    def __repr__(self):
        return f"Quote({self.expr})"


class LambdaNode(ASTNode):
    def __init__(self, params: List[str], body: List[ASTNode], closure_env: 'Environment'):
        self.params = params
        self.body = body
        self.closure_env = closure_env
        self.analyzed = False

    def accept(self, visitor: ASTVisitor) -> Any:
        return visitor.visit_lambda(self)

    def __repr__(self):
        return f"Lambda({self.params}, {len(self.body)} exprs, {len(self.closure_env.symbols)} captured)"


class CallNode(ASTNode):
    def __init__(self, func: ASTNode, args: List[ASTNode]):
        self.func = func
        self.args = args

    def accept(self, visitor: ASTVisitor) -> Any:
        return visitor.visit_call(self)

    def __repr__(self):
        return f"Call({self.func}, {self.args})"


class PrimCallNode(ASTNode):
    def __init__(self, prim_name: str, args: List[ASTNode]):
        self.prim_name = prim_name
        self.args = args

    def accept(self, visitor: ASTVisitor) -> Any:
        return visitor.visit_prim_call(self)

    def __repr__(self):
        return f"PrimCall({self.prim_name}, {self.args})"


class SetqNode(ASTNode):
    def __init__(self, var_name: str, value: ASTNode):
        self.var_name = var_name
        self.value = value

    def accept(self, visitor: ASTVisitor) -> Any:
        return visitor.visit_setq(self)

    def __repr__(self):
        return f"Setq({self.var_name}, {self.value})"


class CondNode(ASTNode):
    def __init__(self, clauses: List[tuple[ASTNode, List[ASTNode]]]):
        self.clauses = clauses

    def accept(self, visitor: ASTVisitor) -> Any:
        return visitor.visit_cond(self)

    def __repr__(self):
        return f"Cond({len(self.clauses)} clauses)"


class ListNode(ASTNode):
    def __init__(self, elements: List[ASTNode]):
        self.elements = elements

    def accept(self, visitor: ASTVisitor) -> Any:
        return visitor.visit_list(self)

    def __repr__(self):
        return f"List({self.elements})"

class DefunNode(ASTNode):
    def __init__(self, name: str, params: List[str], body: List[ASTNode]):
        self.name = name
        self.params = params
        self.body = body

    def accept(self, visitor: ASTVisitor) -> Any:
        return visitor.visit_defun(self)

    def __repr__(self):
        return f"Defun({self.name}, {self.params}, {len(self.body)} exprs)"