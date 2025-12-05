from dataclasses import dataclass, field
from typing import List, Any, TYPE_CHECKING

from .visitor import ASTNode, ASTVisitor

if TYPE_CHECKING:
    from semantic.symbol_table import Environment


@dataclass(frozen=True)
class NilNode(ASTNode):
    def accept(self, visitor: ASTVisitor) -> Any:
        return visitor.visit_nil(self)


@dataclass(frozen=True)
class TrueNode(ASTNode):
    def accept(self, visitor: ASTVisitor) -> Any:
        return visitor.visit_true(self)


@dataclass
class SymbolNode(ASTNode):
    name: str

    def accept(self, visitor: ASTVisitor) -> Any:
        return visitor.visit_symbol(self)


@dataclass
class NumberNode(ASTNode):
    value: float

    def accept(self, visitor: ASTVisitor) -> Any:
        return visitor.visit_number(self)


@dataclass
class StringNode(ASTNode):
    value: str

    def accept(self, visitor: ASTVisitor) -> Any:
        return visitor.visit_string(self)


@dataclass
class QuoteNode(ASTNode):
    expr: ASTNode

    def accept(self, visitor: ASTVisitor) -> Any:
        return visitor.visit_quote(self)


@dataclass
class ListNode(ASTNode):
    elements: List[ASTNode] = field(default_factory=list)

    def accept(self, visitor: ASTVisitor) -> Any:
        return visitor.visit_list(self)


@dataclass
class LambdaNode(ASTNode):
    params: List[str] = field(default_factory=list)
    body: List[ASTNode] = field(default_factory=list)
    closure_env: 'Environment' = field(default=None)

    def accept(self, visitor: ASTVisitor) -> Any:
        return visitor.visit_lambda(self)


@dataclass
class CallNode(ASTNode):
    func: ASTNode
    args: List[ASTNode] = field(default_factory=list)

    def accept(self, visitor: ASTVisitor) -> Any:
        return visitor.visit_call(self)


@dataclass
class PrimCallNode(ASTNode):
    prim_name: str
    args: List[ASTNode] = field(default_factory=list)

    def accept(self, visitor: ASTVisitor) -> Any:
        return visitor.visit_prim_call(self)


@dataclass
class SetqNode(ASTNode):
    var_name: str
    value: ASTNode

    def accept(self, visitor: ASTVisitor) -> Any:
        return visitor.visit_setq(self)


@dataclass
class CondNode(ASTNode):
    clauses: List[tuple[ASTNode, List[ASTNode]]] = field(default_factory=list)

    def accept(self, visitor: ASTVisitor) -> Any:
        return visitor.visit_cond(self)


@dataclass
class DefunNode(ASTNode):
    name: str
    params: List[str] = field(default_factory=list)
    body: List[ASTNode] = field(default_factory=list)

    def accept(self, visitor: ASTVisitor) -> Any:
        return visitor.visit_defun(self)


@dataclass
class PrognNode(ASTNode):
    body: List[ASTNode] = field(default_factory=list)

    def accept(self, visitor: ASTVisitor) -> Any:
        return visitor.visit_progn(self)


@dataclass
class LogicNode(ASTNode):
    op: str
    args: List[ASTNode] = field(default_factory=list)

    def accept(self, visitor: ASTVisitor) -> Any:
        return visitor.visit_logic(self)