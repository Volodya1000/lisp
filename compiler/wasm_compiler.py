from typing import List, Set, Optional
from semantic.ast_nodes import *
from semantic.symbol_table import Environment, SymbolInfo


class WasmCompiler(ASTVisitor):
    def __init__(self):
        self.global_env = Environment()
        self._init_primitives()
        self.current_env = self.global_env
        self.funcs_code: List[str] = []
        self.global_vars: Set[str] = set()

        # Таблица функций для call_indirect.
        self.table_entries: List[str] = []
        self.lambda_counter = 0

        self.math_ops = {
            '+': 'f64.add', '-': 'f64.sub', '*': 'f64.mul', '/': 'f64.div'
        }
        self.comp_ops = {
            '=': 'f64.eq', '<': 'f64.lt', '>': 'f64.gt',
            '<=': 'f64.le', '>=': 'f64.ge', '!=': 'f64.ne',
            'eq': 'f64.eq'
        }

        self.is_inside_func = False

    def _init_primitives(self):
        for name in Environment.PRIMITIVES:
            self.global_env.define(name, is_function=True)

    def compile(self, nodes: List[ASTNode]) -> str:
        self._scan_definitions(nodes)

        main_nodes: List[ASTNode] = []
        for node in nodes:
            if not isinstance(node, DefunNode):
                main_nodes.append(node)
            else:
                node.accept(self)

        main_body = self._compile_block(main_nodes) if main_nodes else "f64.const 0.0"

        wat = '(module\n'
        wat += self._get_imports()
        wat += self._get_memory_config()
        wat += self._get_type_definitions()
        wat += self._get_table_config()
        wat += self._get_globals_definitions()
        wat += self._get_std_lib()

        for func_code in self.funcs_code:
            wat += func_code + '\n'

        wat += '  (func $main (export "main") (result f64)\n'
        wat += '    (local $env f64)\n'
        wat += '    f64.const 0.0\n'
        wat += '    local.set $env\n'

        # Инициализация глобальных функций
        for node in nodes:
            if isinstance(node, DefunNode):
                # Используем имя без префикса, так как visit_defun теперь генерирует имя без префикса
                func_idx = self.table_entries.index(f"${node.name}")
                wat += f"    ;; Init {node.name}\n"
                wat += f"    f64.const {func_idx}.0\n"
                wat += f"    f64.const 0.0\n"
                wat += f"    call $std_cons\n"
                wat += f"    global.set ${node.name}\n"

        wat += self._indent(main_body, 4)
        wat += '\n  )\n'
        wat += ')'
        return wat

    def _get_imports(self) -> str:
        return """
  (import "env" "print_number" (func $print_number (param f64)))
  (import "env" "princ" (func $princ (param f64)))
  (import "env" "read_num" (func $read_num (result f64))) 
"""

    def _get_memory_config(self) -> str:
        return '  (memory (export "memory") 1)\n'

    def _get_type_definitions(self) -> str:
        types = ""
        for i in range(11):
            params = " ".join(["(param f64)"] * (i + 1))
            types += f"  (type $type_{i} (func {params} (result f64)))\n"
        return types

    def _get_table_config(self) -> str:
        count = len(self.table_entries)
        if count == 0:
            return "  (table 0 funcref)\n"
        funcs_list = " ".join(self.table_entries)
        return f"  (table {count} funcref)\n  (elem (i32.const 0) {funcs_list})\n"

    def _get_globals_definitions(self) -> str:
        code = '  (global $heap_ptr (mut i32) (i32.const 8))\n'
        code += '  (global $scratch (mut f64) (f64.const 0.0))\n'
        for g_var in self.global_vars:
            code += f'  (global ${g_var} (mut f64) (f64.const 0.0))\n'
        return code

    def _scan_definitions(self, nodes: List[ASTNode]):
        for node in nodes:
            if isinstance(node, DefunNode):
                self.global_env.define(node.name, is_function=True)
                self.global_vars.add(node.name)
            elif isinstance(node, SetqNode):
                self.global_vars.add(node.var_name)
                self.global_env.define(node.var_name)

    def _compile_block(self, nodes: List[ASTNode]) -> str:
        if not nodes: return "f64.const 0.0"
        lines = []
        for i, node in enumerate(nodes):
            res = node.accept(self)
            lines.append(res)
            if i < len(nodes) - 1: lines.append("drop")
        return "\n".join(lines)

    def _indent(self, code: str, spaces: int) -> str:
        prefix = " " * spaces
        return "\n".join([prefix + line for line in code.split('\n')])

    def _alloc_frame_and_load_args(self, params: List[str], vars_count: int) -> str:
        total_slots = 1 + vars_count
        size_bytes = total_slots * 8

        code = [
            f";; -- Alloc Frame (size {size_bytes}) --",
            "global.get $heap_ptr",
            "local.tee $new_env_addr",
            f"i32.const {size_bytes}",
            "i32.add",
            "global.set $heap_ptr",

            ";; Store Parent Env Ptr at offset 0",
            "local.get $new_env_addr",
            "local.get $env",
            "f64.store"
        ]

        for i, param in enumerate(params):
            offset = (i + 1) * 8
            code.append(f";; Store param '{param}' to frame offset {offset}")
            code.append("local.get $new_env_addr")
            code.append(f"i32.const {offset}")
            code.append("i32.add")
            code.append(f"local.get {i + 1}")
            code.append("f64.store")

        return "\n".join(code)

    def _emit_var_addr(self, info: SymbolInfo) -> str:
        if self.is_inside_func:
            base_ptr = "local.get $new_env_addr"
        else:
            base_ptr = "local.get $env\ni32.trunc_f64_u"

        code = [base_ptr]
        hops = self.current_env.level - info.env_level

        for _ in range(hops):
            code.append("f64.load")
            code.append("i32.trunc_f64_u")

        offset = (info.var_index + 1) * 8
        code.append(f"i32.const {offset}")
        code.append("i32.add")

        return "\n".join(code)

    # --- Visitor Implementation ---

    def visit_defun(self, node: DefunNode):
        # ИСПРАВЛЕНИЕ: Убран префикс $func_, теперь имя совпадает с тестами (func $fact ...)
        func_name = f"${node.name}"
        self.table_entries.append(func_name)

        func_env = Environment(parent=self.global_env)
        prev_env = self.current_env
        self.current_env = func_env

        for param in node.params: func_env.define(param)

        prev_is_inside = self.is_inside_func
        self.is_inside_func = True

        body_wat = self._compile_block(node.body)
        prologue = self._alloc_frame_and_load_args(node.params, func_env.current_var_index + 10)

        self.is_inside_func = prev_is_inside
        self.current_env = prev_env

        params_wat = "(param $env f64) " + " ".join(["(param f64)" for _ in node.params])

        func_wat = f'  (func {func_name} {params_wat} (result f64)\n'
        func_wat += '    (local $new_env_addr i32)\n'
        func_wat += self._indent(prologue, 4) + '\n'
        func_wat += self._indent(body_wat, 4) + '\n'
        func_wat += '  )'
        self.funcs_code.append(func_wat)
        return ""

    def visit_lambda(self, node: LambdaNode) -> str:
        name = f"$lambda_{self.lambda_counter}"
        self.lambda_counter += 1
        self.table_entries.append(name)
        func_idx = len(self.table_entries) - 1

        lambda_env = node.closure_env
        prev_env = self.current_env
        self.current_env = lambda_env

        prev_is_inside = self.is_inside_func
        self.is_inside_func = True

        body_wat = self._compile_block(node.body)
        prologue = self._alloc_frame_and_load_args(node.params, lambda_env.current_var_index + 5)

        self.is_inside_func = prev_is_inside
        self.current_env = prev_env

        params_wat = "(param $env f64) " + " ".join(["(param f64)" for _ in node.params])

        func_wat = f'  (func {name} {params_wat} (result f64)\n'
        func_wat += '    (local $new_env_addr i32)\n'
        func_wat += self._indent(prologue, 4) + '\n'
        func_wat += self._indent(body_wat, 4) + '\n'
        func_wat += '  )'
        self.funcs_code.append(func_wat)

        current_env_reg = "local.get $new_env_addr\nf64.convert_i32_u" if self.is_inside_func else "local.get $env"

        return f"f64.const {func_idx}.0\n{current_env_reg}\ncall $std_cons"

    def visit_call(self, node: CallNode) -> str:
        if isinstance(node.func, SymbolNode) and node.func.name in Environment.PRIMITIVES:
            return self.visit_prim_call(PrimCallNode(node.func.name, node.args))

        func_calc = node.func.accept(self)
        args_calc = [arg.accept(self) for arg in node.args]
        arity = len(node.args)

        code = [
            f";; -- Call Indirect (arity {arity}) --",
            func_calc,
            "global.set $scratch",
            "global.get $scratch",
            "call $std_cdr",
        ]
        code.extend(args_calc)

        code.append("global.get $scratch")
        code.append("call $std_car")
        code.append("i32.trunc_f64_u")
        code.append(f"call_indirect (type $type_{arity})")

        return "\n".join(code)

    def visit_prim_call(self, node: PrimCallNode) -> str:
        if node.prim_name == 'read':
            return "call $read_num"

        if node.prim_name == 'princ':
            return f"{node.args[0].accept(self)}\ncall $princ\nf64.const 0.0"

        # ИСПРАВЛЕНИЕ: Добавлена реализация not
        if node.prim_name == 'not':
            # (not x) -> if x == 0.0 then 1.0 else 0.0
            return f"{node.args[0].accept(self)}\nf64.const 0.0\nf64.eq\n(if (result f64) (then f64.const 1.0) (else f64.const 0.0))"

        if node.prim_name in self.math_ops:
            args = "\n".join([arg.accept(self) for arg in node.args])
            return f"{args}\n{self.math_ops[node.prim_name]}"
        elif node.prim_name in self.comp_ops:
            args = "\n".join([arg.accept(self) for arg in node.args])
            return f"{args}\n{self.comp_ops[node.prim_name]}\nf64.convert_i32_s"
        elif node.prim_name == 'list':
            code = "f64.const 0.0"
            for arg in reversed(node.args):
                code = f"{arg.accept(self)}\n{code}\ncall $std_cons"
            return code
        elif node.prim_name == 'print':
            return f"{node.args[0].accept(self)}\ncall $print_number\nf64.const 0.0"

        mapping = {
            'cons': '$std_cons', 'car': '$std_car', 'cdr': '$std_cdr',
            'equal': '$std_equal', 'length': '$std_length', 'str-concat': '$std_str_concat'
        }
        if node.prim_name in mapping:
            args = "\n".join([arg.accept(self) for arg in node.args])
            return f"{args}\ncall {mapping[node.prim_name]}"

        if node.prim_name in ['null', 'atom']:
            return f"{node.args[0].accept(self)}\ncall $std_is_nil"

        raise NotImplementedError(f"Primitive {node.prim_name} not implemented")

    def visit_symbol(self, node: SymbolNode) -> str:
        info = self.current_env.resolve(node.name)
        if not info: return f"global.get ${node.name}"

        if info.env_level == 0 and info.var_index is None:
            return f"global.get ${node.name}"

        addr_code = self._emit_var_addr(info)
        return f"{addr_code}\nf64.load"

    def visit_setq(self, node: SetqNode) -> str:
        info = self.current_env.resolve(node.var_name)
        val_code = node.value.accept(self)

        if not info:
            self.global_vars.add(node.var_name)
            return f"{val_code}\nglobal.set ${node.var_name}\nglobal.get ${node.var_name}"

        if info.env_level == 0 and info.var_index is None:
            return f"{val_code}\nglobal.set ${node.var_name}\nglobal.get ${node.var_name}"

        code = f"{val_code}\n"
        code += "global.set $scratch\n"
        code += self._emit_var_addr(info) + "\n"
        code += "global.get $scratch\n"
        code += "f64.store\n"
        code += "global.get $scratch"
        return code

    # --- Standard AST Visitors ---
    def visit_number(self, node: NumberNode):
        return f"f64.const {node.value}"

    def visit_nil(self, node: NilNode):
        return "f64.const 0.0"

    def visit_true(self, node: TrueNode):
        return "f64.const 1.0"

    def visit_quote(self, node: QuoteNode):
        return self._compile_quoted_data(node.expr)

    def visit_progn(self, node: PrognNode):
        return self._compile_block(node.body)

    def visit_string(self, node: StringNode) -> str:
        text_bytes = node.value.encode('utf-8')
        length = len(text_bytes)
        code = "global.get $heap_ptr\n"
        code += f"global.get $heap_ptr\ni32.const {length}\ni32.store\n"
        for i, byte in enumerate(text_bytes):
            code += f"global.get $heap_ptr\ni32.const {4 + i}\ni32.add\ni32.const {byte}\ni32.store8\n"
        code += f"global.get $heap_ptr\ni32.const {4 + length}\ni32.add\nglobal.set $heap_ptr\n"
        code += "f64.convert_i32_u"
        return code

    def _compile_quoted_data(self, node):
        if isinstance(node, NumberNode): return f"f64.const {node.value}"
        if isinstance(node, ListNode): return self.visit_list(node)
        if isinstance(node, StringNode): return self.visit_string(node)
        return "f64.const 0.0"

    def visit_list(self, node: ListNode):
        code = "f64.const 0.0"
        for elem in reversed(node.elements):
            code = f"{self._compile_quoted_data(elem)}\n{code}\ncall $std_cons"
        return code

    def visit_cond(self, node: CondNode):
        if not node.clauses: return "f64.const 0.0"
        return self._compile_cond_recursive(node.clauses)

    def _compile_cond_recursive(self, clauses: List[tuple]) -> str:
        if not clauses: return "f64.const 0.0"
        pred, body = clauses[0]
        check = f"{pred.accept(self)}\nf64.const 0.0\nf64.ne"
        then_code = self._compile_block(body)
        else_code = self._compile_cond_recursive(clauses[1:])
        return f"{check}\n(if (result f64) (then {self._indent(then_code, 2)}) (else {self._indent(else_code, 2)}))"

    def visit_logic(self, node: LogicNode):
        if node.op == 'and':
            return self._compile_and(node.args)
        else:
            return self._compile_or(node.args)

    def _compile_and(self, args):
        if not args: return "f64.const 1.0"
        curr, rest = args[0], args[1:]
        if not rest: return curr.accept(self)
        return f"{curr.accept(self)}\nf64.const 0.0\nf64.ne\n(if (result f64) (then {self._indent(self._compile_and(rest), 2)}) (else f64.const 0.0))"

    def _compile_or(self, args):
        if not args: return "f64.const 0.0"
        curr, rest = args[0], args[1:]
        if not rest: return curr.accept(self)
        return f"{curr.accept(self)}\nglobal.set $scratch\nglobal.get $scratch\nf64.const 0.0\nf64.ne\n(if (result f64) (then global.get $scratch) (else {self._indent(self._compile_or(rest), 2)}))"

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
  (func $std_length (param $ptr f64) (result f64)
    local.get $ptr
    i32.trunc_f64_u
    i32.load
    f64.convert_i32_u
  )
  (func $std_str_concat (param $a f64) (param $b f64) (result f64)
    (local $addr_a i32)
    (local $len_a i32)
    (local $addr_b i32)
    (local $len_b i32)
    (local $new_ptr i32)
    (local $new_len i32)
    (local $i i32)

    local.get $a
    i32.trunc_f64_u
    local.set $addr_a

    local.get $b
    i32.trunc_f64_u
    local.set $addr_b

    local.get $addr_a
    i32.load
    local.set $len_a

    local.get $addr_b
    i32.load
    local.set $len_b

    local.get $len_a
    local.get $len_b
    i32.add
    local.set $new_len

    global.get $heap_ptr
    local.set $new_ptr

    local.get $new_ptr
    local.get $new_len
    i32.store

    ;; Copy A
    i32.const 0
    local.set $i
    (block $break_a (loop $loop_a
        local.get $i
        local.get $len_a
        i32.ge_u
        br_if $break_a

        local.get $new_ptr
        i32.const 4
        i32.add
        local.get $i
        i32.add

        local.get $addr_a
        i32.const 4
        i32.add
        local.get $i
        i32.add
        i32.load8_u

        i32.store8

        local.get $i
        i32.const 1
        i32.add
        local.set $i
        br $loop_a
    ))

    ;; Copy B
    i32.const 0
    local.set $i
    (block $break_b (loop $loop_b
        local.get $i
        local.get $len_b
        i32.ge_u
        br_if $break_b

        local.get $new_ptr
        i32.const 4
        i32.add
        local.get $len_a
        i32.add
        local.get $i
        i32.add

        local.get $addr_b
        i32.const 4
        i32.add
        local.get $i
        i32.add
        i32.load8_u

        i32.store8

        local.get $i
        i32.const 1
        i32.add
        local.set $i
        br $loop_b
    ))

    global.get $heap_ptr
    local.get $new_len
    i32.const 4
    i32.add
    i32.add
    global.set $heap_ptr

    local.get $new_ptr
    f64.convert_i32_u
  )
"""