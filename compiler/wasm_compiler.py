from typing import List, Set
from semantic.ast_nodes import *
from semantic.symbol_table import Environment


class WasmCompiler(ASTVisitor):
    def __init__(self):
        self.global_env = Environment()
        self.current_env = self.global_env
        self.funcs_code: List[str] = []
        self.global_vars: Set[str] = set()

    def compile(self, nodes: List[ASTNode]) -> str:
        """
        Сборка WASM модуля.
        Order: Imports -> Memory -> Globals -> Functions
        """
        self._scan_definitions(nodes)

        main_nodes: List[ASTNode] = []
        for node in nodes:
            if isinstance(node, DefunNode):
                node.accept(self)
            else:
                main_nodes.append(node)

        wat = '(module\n'

        # 1. Imports
        wat += '  (import "env" "print_number" (func $print_number (param f64)))\n'

        # 2. Memory
        wat += '  (memory 1)\n'

        # 3. Globals
        wat += '  (global $heap_ptr (mut i32) (i32.const 8))\n'
        for g_var in self.global_vars:
            wat += f'  (global ${g_var} (mut f64) (f64.const 0.0))\n'

        # 4. Std Lib
        wat += self._get_std_lib()

        # 5. Functions
        for func_wat in self.funcs_code:
            wat += func_wat + '\n'

        # 6. Main
        wat += '  (func $main (export "main") (result f64)\n'
        if main_nodes:
            main_code = self._compile_body(main_nodes)
            for line in main_code.split('\n'):
                wat += f'    {line}\n'
        else:
            wat += '    f64.const 0.0\n'
        wat += '  )\n'
        wat += ')'
        return wat

    def _get_std_lib(self) -> str:
        return """
  ;; --- CONS: heap_ptr += 16, store car/cdr ---
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

  ;; --- CAR ---
  (func $std_car (param $ptr f64) (result f64)
    local.get $ptr
    i32.trunc_f64_u
    f64.load
  )

  ;; --- CDR ---
  (func $std_cdr (param $ptr f64) (result f64)
    local.get $ptr
    i32.trunc_f64_u
    i32.const 8
    i32.add
    f64.load
  )

  ;; --- ATOM (Simplified) ---
  ;; Returns 1.0 if x is 0.0 (nil) or seems like a number (not a pointer we alloc)
  ;; For this MVP, we assume pointers are >= 8. But user numbers can be >= 8 too.
  ;; This is ambiguous without Tagged Pointers. 
  ;; Strategy: Check if it equals 0.0 (nil is atom). 
  ;; For now, let's implement basic check: if we used cons, it's a list.
  ;; But we can't distinguish 100.0 from pointer 100.
  ;; TEMPORARY HACK: We won't implement robust 'atom' for numbers vs pointers yet.
  ;; We will implemented (atom nil) -> true.
  (func $std_is_nil (param $x f64) (result f64)
    local.get $x
    f64.const 0.0
    f64.eq
    (if (result f64)
        (then f64.const 1.0)
        (else f64.const 0.0)
    )
  )
"""

    def _scan_definitions(self, nodes: List[ASTNode]):
        for node in nodes:
            if isinstance(node, DefunNode):
                self.global_env.define(node.name, is_function=True)
            elif isinstance(node, SetqNode):
                self.global_vars.add(node.var_name)
                self.global_env.define(node.var_name, is_function=False)

    # --- Visitor Implementation ---

    def visit_defun(self, node: DefunNode):
        func_env = Environment(parent=self.global_env)
        previous_env = self.current_env
        self.current_env = func_env
        for param_name in node.params:
            self.current_env.define_wasm_local(param_name)
        body_wat = self._compile_body(node.body)
        self.current_env = previous_env
        params_wat = " ".join(["(param f64)" for _ in node.params])
        func_wat = f'  (func ${node.name} {params_wat} (result f64)\n'
        for line in body_wat.split('\n'):
            func_wat += f'    {line}\n'
        func_wat += '  )'
        self.funcs_code.append(func_wat)
        return ""

    def visit_call(self, node: CallNode) -> str:
        func_name = node.func.name if isinstance(node.func, SymbolNode) else None
        if not func_name: raise NotImplementedError("Indirect calls not supported")
        args_code = [arg.accept(self) for arg in node.args]
        return "\n    ".join(args_code) + f"\n    call ${func_name}"

    def visit_symbol(self, node: SymbolNode) -> str:
        info = self.current_env.resolve(node.name)
        if not info: raise RuntimeError(f"Символ не найден: {node.name}")
        if info.wasm_loc_idx is not None:
            return f"local.get {info.wasm_loc_idx}"
        elif not info.is_function:
            if node.name not in self.global_vars: self.global_vars.add(node.name)
            return f"global.get ${node.name}"
        else:
            raise RuntimeError(f"Function as var: {node.name}")

    def visit_setq(self, node: SetqNode) -> str:
        val_code = node.value.accept(self)
        info = self.current_env.resolve(node.var_name)
        if not info:
            self.global_vars.add(node.var_name)
            info = self.global_env.define(node.var_name)
        if info.wasm_loc_idx is not None:
            return f"{val_code}\n    local.tee {info.wasm_loc_idx}"
        else:
            return f"{val_code}\n    global.set ${node.var_name}\n    global.get ${node.var_name}"

    def visit_number(self, node: NumberNode) -> str:
        return f"f64.const {node.value}"

    def visit_nil(self, node: NilNode) -> str:
        return "f64.const 0.0"

    def visit_true(self, node: TrueNode) -> str:
        return "f64.const 1.0"

    # --- Новая реализация visit_quote и list ---

    def visit_quote(self, node: QuoteNode) -> str:
        """
        Рекурсивно компилирует данные внутри quote.
        '(1 2) -> (cons 1 (cons 2 nil))
        """
        return self._compile_quoted_data(node.expr)

    def _compile_quoted_data(self, node: ASTNode) -> str:
        if isinstance(node, NumberNode):
            return f"f64.const {node.value}"
        elif isinstance(node, NilNode):
            return "f64.const 0.0"
        elif isinstance(node, ListNode):
            # Рекурсивно создаем цепочку cons
            if not node.elements:
                return "f64.const 0.0"  # empty list is nil

            # (cons head (cons ...))
            code = ""
            # Идем с конца, чтобы вкладывать вызовы
            # Но WASM стековый. Нам нужно положить HEAD, потом TAIL, потом CALL.
            # (cons 1 (cons 2 nil))
            # Stack: [1] -> [1, 2] -> [1, 2, nil] -> [1, cons(2,nil)] -> cons(...)

            head = node.elements[0]
            tail_elements = node.elements[1:]

            head_code = self._compile_quoted_data(head)

            # Создаем фиктивный ListNode для хвоста
            tail_node = ListNode(tail_elements) if tail_elements else NilNode()
            tail_code = self._compile_quoted_data(tail_node)

            return f"{head_code}\n    {tail_code}\n    call $std_cons"

        elif isinstance(node, SymbolNode):
            # В MVP мы не поддерживаем символы времени выполнения (как строки).
            # Просто вернем 0.0 или ошибку.
            raise NotImplementedError(f"Quoted symbols not fully supported in MVP: {node.name}")
        else:
            raise NotImplementedError(f"Cannot quote type: {type(node)}")

    def visit_prim_call(self, node: PrimCallNode) -> str:
        op_map = {
            '+': 'f64.add', '-': 'f64.sub', '*': 'f64.mul', '/': 'f64.div',
            '=': 'f64.eq', '<': 'f64.lt', '>': 'f64.gt', '<=': 'f64.le', '>=': 'f64.ge'
        }

        # --- List Creation ---
        if node.prim_name == 'list':
            # (list a b) -> (cons a (cons b nil))
            # Генерируем код: push a, push b, push nil, call cons, call cons
            if not node.args:
                return "f64.const 0.0"

            # Начинаем с nil на стеке
            code = "f64.const 0.0"

            # Проходим по аргументам в обратном порядке
            for arg in reversed(node.args):
                arg_code = arg.accept(self)
                # Stack state before: [tail_ptr]
                # We need: [arg_val, tail_ptr] -> call cons -> [new_cons_ptr]
                # WASM stack order for cons: param 1 (car), param 2 (cdr)
                # Сейчас на стеке CDR. Нам нужно подложить CAR под него?
                # Нет. Порядок инструкций:
                # 1. Вычислить CAR
                # 2. (Уже на стеке) CDR
                # call $std_cons

                # Стоп. Если я иду с конца:
                # 1. (list 1 2)
                # 2. Push nil (tail of 2)
                # 3. Calc 2. Stack: [nil, 2]. Cons needs [2, nil].
                # Значит, порядок вычисления:
                # code = arg.accept(self) + code + call

                code = f"{arg_code}\n    {code}\n    call $std_cons"
            return code

        # --- Memory ---
        if node.prim_name == 'cons':
            return f"{node.args[0].accept(self)}\n    {node.args[1].accept(self)}\n    call $std_cons"
        if node.prim_name == 'car':
            return f"{node.args[0].accept(self)}\n    call $std_car"
        if node.prim_name == 'cdr':
            return f"{node.args[0].accept(self)}\n    call $std_cdr"

        # --- Atom/Misc ---
        if node.prim_name == 'atom':
            # (atom x) -> true if x is nil.
            # In full Lisp, true if number or symbol or nil.
            # Simplified: check if nil (0.0).
            # Note: This is weak because 0.0 is also a number.
            return f"{node.args[0].accept(self)}\n    call $std_is_nil"

        if node.prim_name == 'print':
            return f"{node.args[0].accept(self)}\n    call $print_number\n    f64.const 0.0"

        if node.prim_name == 'not':
            return f"{node.args[0].accept(self)}\n    f64.const 0.0\n    f64.eq\n    f64.convert_i32_s"

        if node.prim_name not in op_map:
            raise NotImplementedError(f"Primitive '{node.prim_name}' not supported")

        args_code = "\n    ".join([arg.accept(self) for arg in node.args])
        code = f"{args_code}\n    {op_map[node.prim_name]}"
        if node.prim_name in ['=', '<', '>', '<=', '>=']:
            code += "\n    f64.convert_i32_s"
        return code

    def visit_cond(self, node: CondNode) -> str:
        if not node.clauses: return "f64.const 0.0"
        pred_node, body_nodes = node.clauses[0]
        if isinstance(pred_node, TrueNode) or (isinstance(pred_node, SymbolNode) and pred_node.name == 't'):
            return self._compile_body(body_nodes)

        code = pred_node.accept(self)
        code += "\n    f64.const 0.0\n    f64.ne"
        code += "\n    (if (result f64)\n      (then\n        " + self._compile_body(body_nodes).replace("\n",
                                                                                                         "\n        ") + "\n      )"
        rest = node.clauses[1:]
        if rest:
            code += "\n      (else\n        " + self.visit_cond(CondNode(rest)).replace("\n",
                                                                                        "\n        ") + "\n      )"
        else:
            code += "\n      (else f64.const 0.0)"
        code += "\n    )"
        return code

    def _compile_body(self, nodes: List[ASTNode]) -> str:
        if not nodes: return "f64.const 0.0"
        lines = []
        for i in range(len(nodes) - 1):
            lines.append(nodes[i].accept(self))
            lines.append("    drop")
        lines.append(nodes[-1].accept(self))
        return "\n".join(lines)

    # --- Заглушки ---
    def visit_string(self, node):
        raise NotImplementedError("Strings not supported")

    def visit_list(self, node):
        raise NotImplementedError("Raw lists not supported in visitor (should be quoted)")

    def visit_lambda(self, node):
        raise NotImplementedError("Lambdas not supported")