import logging
import sys
from typing import List

# Импорты хендлеров
from .handlers.variables import VariableHandler
from .handlers.functions import FunctionHandler
from .handlers.control_flow import ControlFlowHandler
from .handlers.values import ValueHandler

from .wasm_context import CompilerContext
from .wasm_stdlib import WasmStdLib
from .wasm_primitives import PrimitiveHandler
from .wat_builder import WatBuilder
from semantic.ast_nodes import *
from semantic.symbol_table import Environment  # <--- БЫЛ ПРОПУЩЕН ЭТОТ ИМПОРТ

logging.basicConfig(level=logging.DEBUG, format='[%(levelname)s] %(message)s', stream=sys.stderr)
logger = logging.getLogger("WasmCompiler")


class WasmCompiler(ASTVisitor):
    def __init__(self):
        self.global_env = Environment()
        self._init_primitives()
        self.ctx = CompilerContext(self.global_env)

        # Инициализация обработчиков с передачей контекста
        # Используем type() для создания простого объекта-контейнера (Namespace)
        self.handlers = type('Handlers', (), {})()
        self.handlers.variables = VariableHandler(self.ctx)
        self.handlers.functions = FunctionHandler(self.ctx)
        self.handlers.control_flow = ControlFlowHandler(self.ctx)
        self.handlers.values = ValueHandler(self.ctx)

        # Обратная ссылка в контексте на хендлеры (для удобства кросс-вызовов)
        self.ctx.handlers = self.handlers

        self.prim_handler = PrimitiveHandler()

    def _init_primitives(self):
        for name in Environment.PRIMITIVES:
            self.global_env.define(name, is_function=True)

    def compile(self, nodes: List[ASTNode]) -> str:
        logger.debug("Starting compilation...")
        self.ctx.type_registry.registered_types.clear()

        # 1. Предварительный проход: регистрация глобальных имен
        self._scan_definitions(nodes)

        # 2. Разделение на функции и основной код
        main_nodes = []
        for node in nodes:
            if isinstance(node, DefunNode):
                node.accept(self)  # Компилирует и регистрирует функцию
            else:
                main_nodes.append(node)

        # 3. Компиляция тела main
        main_body = self.handlers.control_flow.handle_progn(main_nodes, self)

        # 4. Сборка финального модуля
        return self._build_final_module(nodes, main_body)

    def _scan_definitions(self, nodes: List[ASTNode]):
        for node in nodes:
            if isinstance(node, DefunNode):
                self.global_env.define(node.name, is_function=True)
                self.ctx.global_vars.add(node.name)
            elif isinstance(node, SetqNode):
                self.ctx.define_global(node.var_name)

    # --- Visitor Implementation (Routing) ---

    def visit_number(self, node: NumberNode):
        return self.handlers.values.handle_number(node)

    def visit_nil(self, node: NilNode):
        return self.handlers.values.handle_nil()

    def visit_true(self, node: TrueNode):
        return self.handlers.values.handle_true()

    def visit_string(self, node: StringNode):
        return self.handlers.values.handle_string(node)

    def visit_quote(self, node: QuoteNode):
        return self.handlers.values.handle_quote(node, self)

    def visit_list(self, node: ListNode):
        return self.handlers.values.handle_list(node, self)

    def visit_symbol(self, node: SymbolNode):
        return self.handlers.variables.handle_get(node.name)

    def visit_setq(self, node: SetqNode):
        return self.handlers.variables.handle_set(node.var_name, node.value, self)

    def visit_defun(self, node: DefunNode):
        return self.handlers.functions.handle_defun(node, self)

    def visit_lambda(self, node: LambdaNode):
        return self.handlers.functions.handle_lambda(node, self)

    def visit_call(self, node: CallNode):
        return self.handlers.functions.handle_call(node, self)

    def visit_if(self, node: CondNode):
        return self.handlers.control_flow.handle_if(node.condition, node.then_branch, node.else_branch, self)

    def visit_cond(self, node: CondNode):
        return self.handlers.control_flow.handle_cond(node.clauses, self)

    def visit_logic(self, node: LogicNode):
        return self.handlers.control_flow.handle_logic(node.op, node.args, self)

    def visit_progn(self, node: PrognNode):
        return self.handlers.control_flow.handle_progn(node.body, self)

    def visit_prim_call(self, node):
        return self.prim_handler.handle(node.prim_name, node.args, self)

    # --- Module Assembly ---

    def _build_final_module(self, nodes, main_body):
        wb = WatBuilder()
        wb.raw('(module')
        wb.raw(WasmStdLib.get_imports())
        wb.raw(WasmStdLib.get_memory_config())

        # Типы и Таблицы
        wb.raw(self.ctx.type_registry.generate_definitions())
        wb.raw(self._get_table_config())

        # Глобальные переменные и рантайм
        wb.raw(WasmStdLib.get_globals(self.ctx.global_vars))
        wb.raw(WasmStdLib.get_runtime_funcs())

        # Скомпилированные функции
        for func_code in self.ctx.funcs_code:
            wb.raw(func_code)

        # Main function
        wb.raw(self._generate_main_func(nodes, main_body))
        wb.raw(')')
        return wb.build()

    def _generate_main_func(self, nodes, main_body):
        wb = WatBuilder()
        wb.raw('  (func $entry (export "main") (result f64)')
        wb.raw('    (local $env f64)')

        # Доступ к приватному методу допустим, т.к. мы знаем структуру
        wb.raw(self.handlers.functions._get_closure_locals_defs())

        wb.emit_const('0.0')
        wb.emit_set('$env')

        # Инициализация глобальных функций (привязка замыканий к глобальным переменным)
        for node in nodes:
            if isinstance(node, DefunNode):
                try:
                    func_idx = self.ctx.table_entries.index(f"${node.name}")
                    wb.raw(f"    ;; Init {node.name}")
                    wb.emit_const(f"{func_idx}.0")
                    wb.emit_const('0.0')
                    wb.emit_call('$std_cons')
                    wb.emit_set(f"${node.name}", 'global')
                except ValueError:
                    pass

        wb.raw(main_body)
        wb.raw('  )')
        return wb.build()

    def _get_table_config(self) -> str:
        count = len(self.ctx.table_entries)
        if count == 0: return "  (table 0 funcref)\n"
        funcs_list = " ".join(self.ctx.table_entries)
        return f"  (table {count} funcref)\n  (elem (i32.const 0) {funcs_list})\n"