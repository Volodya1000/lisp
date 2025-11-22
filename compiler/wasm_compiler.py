from typing import List, Set
from semantic.ast_nodes import *
from semantic.symbol_table import Environment


class WasmCompiler(ASTVisitor):
    def __init__(self):
        self.global_env = Environment()
        self.current_env = self.global_env
        self.funcs_code: List[str] = []
        self.global_vars: Set[str] = set()

        self.math_ops = {
            '+': 'f64.add', '-': 'f64.sub', '*': 'f64.mul', '/': 'f64.div'
        }
        self.comp_ops = {
            '=': 'f64.eq', '<': 'f64.lt', '>': 'f64.gt',
            '<=': 'f64.le', '>=': 'f64.ge', '!=': 'f64.ne'
        }

    def compile(self, nodes: List[ASTNode]) -> str:
        self._scan_definitions(nodes)

        main_nodes: List[ASTNode] = []
        for node in nodes:
            if isinstance(node, DefunNode):
                node.accept(self)
            else:
                main_nodes.append(node)

        wat = '(module\n'
        wat += self._get_imports()
        wat += self._get_memory_config()
        wat += self._get_globals_definitions()
        wat += self._get_std_lib()

        for func_code in self.funcs_code:
            wat += func_code + '\n'

        wat += '  (func $main (export "main") (result f64)\n'
        if main_nodes:
            main_body = self._compile_block(main_nodes)
            wat += self._indent(main_body, 4)
        else:
            wat += '    f64.const 0.0\n'
        wat += '  )\n'
        wat += ')'
        return wat

    def _get_imports(self) -> str:
        return """
  (import "env" "print_number" (func $print_number (param f64)))
  (import "env" "princ" (func $princ (param f64)))
"""

    def _get_memory_config(self) -> str:
        #  Экспортируем память под именем "memory"
        return '  (memory (export "memory") 1)\n'
    def _get_globals_definitions(self) -> str:
        code = '  (global $heap_ptr (mut i32) (i32.const 8))\n'
        for g_var in self.global_vars:
            code += f'  (global ${g_var} (mut f64) (f64.const 0.0))\n'
        return code

    def _get_std_lib(self) -> str:
        return """
  (func $std_cons (param $car f64) (param $cdr f64) (result f64)
    (local $addr i32)
    global.get $heap_ptr
    local.tee $addr
    i32.const 16
    i32.add
    global.set $heap_ptr
    local.get $addr
    local.get $car
    f64.store
    local.get $addr
    i32.const 8
    i32.add
    local.get $cdr
    f64.store
    local.get $addr
    f64.convert_i32_u
  )
  (func $std_car (param $ptr f64) (result f64)
    local.get $ptr
    i32.trunc_f64_u
    f64.load
  )
  (func $std_cdr (param $ptr f64) (result f64)
    local.get $ptr
    i32.trunc_f64_u
    i32.const 8
    i32.add
    f64.load
  )
  (func $std_is_nil (param $x f64) (result f64)
    local.get $x
    f64.const 0.0
    f64.eq
    (if (result f64) (then f64.const 1.0) (else f64.const 0.0))
  )
  (func $std_equal (param $a f64) (param $b f64) (result f64)
    (local $eq_base i32)
    local.get $a
    local.get $b
    f64.eq
    local.tee $eq_base
    if (result f64)
        f64.const 1.0
    else
        local.get $a
        f64.const 0.0
        f64.eq
        if (result f64)
            f64.const 0.0
        else
            local.get $b
            f64.const 0.0
            f64.eq
            if (result f64)
                 f64.const 0.0
            else
                 local.get $a
                 call $std_car
                 local.get $b
                 call $std_car
                 call $std_equal
                 f64.const 0.0
                 f64.eq
                 if (result f64)
                    f64.const 0.0
                 else
                    local.get $a
                    call $std_cdr
                    local.get $b
                    call $std_cdr
                    call $std_equal
                 end
            end
        end
    end
  )
"""

    def _scan_definitions(self, nodes: List[ASTNode]):
        for node in nodes:
            if isinstance(node, DefunNode):
                self.global_env.define(node.name, is_function=True)
            elif isinstance(node, SetqNode):
                self.global_vars.add(node.var_name)
                self.global_env.define(node.var_name, is_function=False)

    def _compile_block(self, nodes: List[ASTNode]) -> str:
        if not nodes:
            return "f64.const 0.0"

        lines = []
        for i, node in enumerate(nodes):
            res = node.accept(self)
            if res is None:
                raise RuntimeError(f"Compilation failed (returned None) for node: {node}")
            lines.append(res)
            if i < len(nodes) - 1:
                lines.append("drop")
        return "\n".join(lines)

    def _indent(self, code: str, spaces: int) -> str:
        prefix = " " * spaces
        return "\n".join([prefix + line for line in code.split('\n')])

    # --- Visitor Implementation ---

    def visit_defun(self, node: DefunNode):
        func_env = Environment(parent=self.global_env)
        previous_env = self.current_env
        self.current_env = func_env
        for param_name in node.params:
            self.current_env.define_wasm_local(param_name)
        body_wat = self._compile_block(node.body)
        self.current_env = previous_env
        params_wat = " ".join(["(param f64)" for _ in node.params])
        func_wat = f'  (func ${node.name} {params_wat} (result f64)\n'
        func_wat += self._indent(body_wat, 4) + '\n'
        func_wat += '  )'
        self.funcs_code.append(func_wat)
        return ""

    def visit_call(self, node: CallNode) -> str:
        func_name = node.func.name if isinstance(node.func, SymbolNode) else None

        # --- Поддержка princ ---
        if func_name == 'princ':
            arg_code = node.args[0].accept(self)
            return f"{arg_code}\ncall $princ\nf64.const 0.0"

        if not func_name:
            raise NotImplementedError("Indirect calls not supported in MVP")

        args_code = "\n".join([arg.accept(self) for arg in node.args])
        return f"{args_code}\ncall ${func_name}"

    def visit_setq(self, node: SetqNode) -> str:
        val_code = node.value.accept(self)
        info = self.current_env.resolve(node.var_name)
        if not info:
            self.global_vars.add(node.var_name)
            info = self.global_env.define(node.var_name)

        if info.wasm_loc_idx is not None:
            return f"{val_code}\nlocal.tee {info.wasm_loc_idx}"
        else:
            return f"{val_code}\nglobal.set ${node.var_name}\nglobal.get ${node.var_name}"

    def visit_symbol(self, node: SymbolNode) -> str:
        info = self.current_env.resolve(node.name)
        if not info:
            raise RuntimeError(f"Undefined symbol: {node.name}")
        if info.wasm_loc_idx is not None:
            return f"local.get {info.wasm_loc_idx}"
        elif info.is_function:
            raise RuntimeError(f"Cannot use function '{node.name}' as variable")
        else:
            if node.name not in self.global_vars:
                self.global_vars.add(node.name)
            return f"global.get ${node.name}"

    def visit_number(self, node: NumberNode) -> str:
        return f"f64.const {node.value}"

    # --- РЕАЛИЗАЦИЯ СТРОК ---
    def visit_string(self, node: StringNode) -> str:
        text_bytes = node.value.encode('utf-8')
        length = len(text_bytes)

        # 1. Берем текущий heap_ptr (это будет адрес строки)
        # Стек: [ptr]
        code = "global.get $heap_ptr\n"

        # 2. Пишем длину
        code += f"global.get $heap_ptr\n"
        code += f"i32.const {length}\n"
        code += f"i32.store\n"

        # 3. Пишем байты
        for i, byte in enumerate(text_bytes):
            code += f"global.get $heap_ptr\n"
            code += f"i32.const {4 + i}\n"
            code += f"i32.add\n"
            code += f"i32.const {byte}\n"
            code += f"i32.store8\n"

        # 4. Двигаем heap_ptr
        aligned_len = 4 + length
        code += f"global.get $heap_ptr\n"
        code += f"i32.const {aligned_len}\n"
        code += f"i32.add\n"
        code += f"global.set $heap_ptr\n"

        # 5. Превращаем результат (ptr) в f64
        code += "f64.convert_i32_u"

        return code

    def visit_nil(self, node: NilNode) -> str:
        return "f64.const 0.0"

    def visit_true(self, node: TrueNode) -> str:
        return "f64.const 1.0"

    def visit_quote(self, node: QuoteNode) -> str:
        return self._compile_quoted_data(node.expr)

    def _compile_quoted_data(self, node: ASTNode) -> str:
        if isinstance(node, NumberNode):
            return f"f64.const {node.value}"
        elif isinstance(node, NilNode):
            return "f64.const 0.0"
        elif isinstance(node, StringNode):
            return self.visit_string(node)
        elif isinstance(node, ListNode):
            if not node.elements:
                return "f64.const 0.0"
            head = node.elements[0]
            tail_elements = node.elements[1:]
            head_code = self._compile_quoted_data(head)
            if tail_elements:
                tail_node = ListNode(tail_elements)
                tail_code = self._compile_quoted_data(tail_node)
            else:
                tail_code = "f64.const 0.0"
            return f"{head_code}\n{tail_code}\ncall $std_cons"
        else:
            return "f64.const 0.0"

    def visit_prim_call(self, node: PrimCallNode) -> str:
        if node.prim_name in self.math_ops:
            return self._compile_math_op(node)
        elif node.prim_name in self.comp_ops:
            return self._compile_comp_op(node)
        elif node.prim_name in ['cons', 'car', 'cdr', 'list', 'equal', 'atom', 'null']:
            return self._compile_list_op(node)
        elif node.prim_name == 'not':
            return self._compile_not_op(node)
        elif node.prim_name == 'print':
            return f"{node.args[0].accept(self)}\ncall $print_number\nf64.const 0.0"
        else:
            raise NotImplementedError(f"Primitive {node.prim_name} not implemented")

    def _compile_math_op(self, node: PrimCallNode) -> str:
        args_code = "\n".join([arg.accept(self) for arg in node.args])
        op = self.math_ops[node.prim_name]
        return f"{args_code}\n{op}"

    def _compile_comp_op(self, node: PrimCallNode) -> str:
        args_code = "\n".join([arg.accept(self) for arg in node.args])
        op = self.comp_ops[node.prim_name]
        return f"{args_code}\n{op}\nf64.convert_i32_s"

    def _compile_not_op(self, node: PrimCallNode) -> str:
        val = node.args[0].accept(self)
        return f"{val}\nf64.const 0.0\nf64.eq\nf64.convert_i32_s"

    def _compile_list_op(self, node: PrimCallNode) -> str:
        name = node.prim_name
        if name == 'list':
            code = "f64.const 0.0"
            for arg in reversed(node.args):
                code = f"{arg.accept(self)}\n{code}\ncall $std_cons"
            return code

        args = "\n".join([arg.accept(self) for arg in node.args])

        if name == 'cons': return f"{args}\ncall $std_cons"
        if name == 'car': return f"{args}\ncall $std_car"
        if name == 'cdr': return f"{args}\ncall $std_cdr"
        if name == 'equal': return f"{args}\ncall $std_equal"
        if name == 'null' or name == 'atom':
            return f"{args}\ncall $std_is_nil"

        return ""

    def visit_cond(self, node: CondNode) -> str:
        if not node.clauses:
            return "f64.const 0.0"
        return self._compile_cond_recursive(node.clauses)

    def _compile_cond_recursive(self, clauses: List[tuple]) -> str:
        if not clauses:
            return "f64.const 0.0"

        pred_node, body_nodes = clauses[0]

        if isinstance(pred_node, TrueNode) or (isinstance(pred_node, SymbolNode) and pred_node.name == 't'):
            return self._compile_block(body_nodes)

        pred_code = pred_node.accept(self)
        check_code = f"{pred_code}\nf64.const 0.0\nf64.ne"

        then_code = self._compile_block(body_nodes)
        else_code = self._compile_cond_recursive(clauses[1:])

        return f"{check_code}\n(if (result f64)\n(then\n{self._indent(then_code, 2)}\n)\n(else\n{self._indent(else_code, 2)}\n)\n)"

    def visit_list(self, node):
        raise NotImplementedError("Raw lists not supported")

    def visit_lambda(self, node):
        raise NotImplementedError("Lambdas not yet supported")