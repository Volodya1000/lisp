from typing import Dict
from semantic.symbol_info import SymbolInfo, SymbolKind


BUILTINS: Dict[str, SymbolInfo] = {
    "+": SymbolInfo("+", SymbolKind.BUILTIN, param_count=None),
    "-": SymbolInfo("-", SymbolKind.BUILTIN, param_count=None),
    "*": SymbolInfo("*", SymbolKind.BUILTIN, param_count=None),
    "/": SymbolInfo("/", SymbolKind.BUILTIN, param_count=2),
    "car": SymbolInfo("car", SymbolKind.BUILTIN, param_count=1),
    "cdr": SymbolInfo("cdr", SymbolKind.BUILTIN, param_count=1),
    "cons": SymbolInfo("cons", SymbolKind.BUILTIN, param_count=2),
    "list": SymbolInfo("list", SymbolKind.BUILTIN, param_count=None),
    "=": SymbolInfo("=", SymbolKind.BUILTIN, param_count=2),
}