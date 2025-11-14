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
    "format": SymbolInfo("format", SymbolKind.BUILTIN, param_count=None),
    "read-line": SymbolInfo("read-line", SymbolKind.BUILTIN, param_count=0),
    "read": SymbolInfo("read", SymbolKind.BUILTIN, param_count=0),
    "parse-integer": SymbolInfo("parse-integer", SymbolKind.BUILTIN, param_count=1),
    "read-from-string": SymbolInfo("read-from-string", SymbolKind.BUILTIN, param_count=1),
    "print": SymbolInfo("print", SymbolKind.BUILTIN, param_count=1),
    "zerop": SymbolInfo("zerop", SymbolKind.BUILTIN, param_count=1),

}