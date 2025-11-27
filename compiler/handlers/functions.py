import logging
from typing import List
from .base import BaseHandler
from ..wasm_types import WasmType, OpCode
from ..frame_policy import FramePolicy
from semantic.ast_nodes import DefunNode, LambdaNode, CallNode, SymbolNode
from semantic.symbol_table import Environment

logger = logging.getLogger(__name__)


class FunctionHandler(BaseHandler):

    def handle_defun(self, node: DefunNode, visitor) -> str:
        func_name = f"${node.name}"
        self.ctx.register_function(func_name)

        func_env = Environment(parent=self.ctx.global_env)
        for param in node.params:
            func_env.define(param)

        func_code = self._compile_function_body(func_name, node.params, node.body, func_env, visitor)
        self.ctx.funcs_code.append(func_code)
        return ""

    def handle_lambda(self, node: LambdaNode, visitor) -> str:
        name = self.ctx.get_lambda_name()
        func_idx = self.ctx.register_function(name)

        func_code = self._compile_function_body(name, node.params, node.body, node.closure_env, visitor)
        self.ctx.funcs_code.append(func_code)

        wb = self.get_builder()
        wb.emit_const(f"{func_idx}.0")

        # Захват окружения
        if self.ctx.is_inside_func:
            wb.emit_get('$new_env_addr')
            wb.emit(OpCode.CONVERT_U, WasmType.F64)
        else:
            wb.emit_get('$env')  # В main env=0.0

        wb.emit_call('$std_cons')
        return wb.build()

    def handle_call(self, node: CallNode, visitor) -> str:
        if isinstance(node.func, SymbolNode) and node.func.name in Environment.PRIMITIVES:
            from semantic.ast_nodes import PrimCallNode
            return visitor.visit_prim_call(PrimCallNode(node.func.name, node.args))

        if isinstance(node.func, SymbolNode) and node.func.name == 'if':
            if len(node.args) < 2: raise Exception("if requires at least 2 args")
            return self.ctx.handlers.control_flow.handle_if(
                node.args[0], node.args[1], node.args[2] if len(node.args) > 2 else None, visitor
            )

        return self._compile_indirect_call(node, visitor)

    def _compile_function_body(self, name: str, params: List[str], body: List, env: Environment, visitor) -> str:
        prev_env = self.ctx.current_env
        prev_is_inside = self.ctx.is_inside_func

        self.ctx.enter_function(env)

        body_wat = self.ctx.handlers.control_flow.handle_progn(body, visitor)

        frame_size = FramePolicy.calculate_frame_size(env.current_var_index + 10)
        prologue = self._generate_prologue(params, frame_size)

        self.ctx.exit_function(prev_env, prev_is_inside)

        arity = len(params)
        self.ctx.type_registry.get_or_register(arity)

        params_wat = "(param $env f64) " + " ".join([f"(param {WasmType.F64})" for _ in params])

        wb = self.get_builder()
        wb.raw(f'  (func {name} {params_wat} (result f64)')
        wb.raw('    (local $new_env_addr i32)')
        wb.raw(self._get_closure_locals_defs())
        wb.raw(self._indent(prologue, 4))
        wb.raw(self._indent(body_wat, 4))
        wb.raw('  )')
        return wb.build()

    def _generate_prologue(self, params: List[str], size_bytes: int) -> str:
        wb = self.get_builder()
        wb.raw(f";; -- Prologue: Alloc Frame (size {size_bytes}) --")

        wb.emit_get('$heap_ptr', 'global')

        # Исправлено: используем raw для tee с аргументом
        wb.raw("local.tee $new_env_addr")

        wb.emit_const(size_bytes, WasmType.I32)
        wb.emit(OpCode.ADD, WasmType.I32)
        wb.emit_set('$heap_ptr', 'global')

        # Сохранение ссылки на родительское окружение (offset 0)
        wb.emit_get('$new_env_addr')
        wb.emit_get('$env')
        wb.emit(OpCode.STORE, WasmType.F64)

        # Сохранение аргументов
        for i, param in enumerate(params):
            offset = FramePolicy.get_var_offset(i)
            wb.emit_get('$new_env_addr')
            wb.emit_const(offset, WasmType.I32)
            wb.emit(OpCode.ADD, WasmType.I32)
            # param 0 - env, params 1..N - args
            # local.get index (индексы locals начинаются с 0)
            # $env - 0, arg1 - 1, arg2 - 2...
            wb.emit_get(f"{i + 1}", 'local')
            wb.emit(OpCode.STORE, WasmType.F64)

        return wb.build()

    def _compile_indirect_call(self, node: CallNode, visitor) -> str:
        wb = self.get_builder()

        if self.ctx.call_depth >= 20:
            raise Exception("Max call nesting depth exceeded")

        closure_local = f"$closure_{self.ctx.call_depth}"
        self.ctx.call_depth += 1

        try:
            if isinstance(node.func, SymbolNode) and node.func.name == 'funcall':
                func_expr = node.args[0]
                call_args = node.args[1:]
            else:
                func_expr = node.func
                call_args = node.args

            arity = len(call_args)
            type_name = self.ctx.type_registry.get_or_register(arity)

            wb.raw(func_expr.accept(visitor))
            wb.emit_set(closure_local)

            wb.emit_get(closure_local)
            wb.emit_call('$std_cdr')

            for arg in call_args:
                wb.raw(arg.accept(visitor))

            wb.emit_get(closure_local)
            wb.emit_call('$std_car')
            wb.emit(OpCode.TRUNC_U, WasmType.I32)  # Используем OpCode.TRUNC_U

            wb.raw(f"call_indirect (type {type_name})")
            return wb.build()
        finally:
            self.ctx.call_depth -= 1

    def _get_closure_locals_defs(self) -> str:
        return "\n".join([f"    (local $closure_{i} f64)" for i in range(20)])

    def _indent(self, text: str, spaces: int) -> str:
        pad = " " * spaces
        return "\n".join([pad + line for line in text.split('\n')])