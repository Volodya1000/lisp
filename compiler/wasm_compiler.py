from typing import List, Set
from semantic.ast_nodes import *
from semantic.symbol_table import Environment


class WasmCompiler(ASTVisitor):
    def __init__(self):
        self.global_env = Environment()
        self.current_env = self.global_env
        # Хранилище сгенерированного кода функций
        self.funcs_code: List[str] = []
        # Множество имен глобальных переменных
        self.global_vars: Set[str] = set()

    def compile(self, nodes: List[ASTNode]) -> str:
        """
        Сборка WASM модуля.
        ВАЖНО: Порядок секций строго определен стандартом WASM.
        1. Imports
        2. Memory
        3. Globals
        4. Functions
        """
        # 1. Сканируем определения (defun, setq)
        self._scan_definitions(nodes)

        # Разделяем функции и код main
        main_nodes: List[ASTNode] = []
        for node in nodes:
            if isinstance(node, DefunNode):
                node.accept(self)
            else:
                main_nodes.append(node)

        wat = '(module\n'

        # --- 1. Imports ---
        wat += '  (import "env" "print_number" (func $print_number (param f64)))\n'

        # --- 2. Memory ---
        # 1 страница = 64KB
        wat += '  (memory 1)\n'

        # --- 3. Globals ---
        # Системный указатель кучи (начинаем с 8, чтобы 0 был null)
        wat += '  (global $heap_ptr (mut i32) (i32.const 8))\n'

        # Пользовательские глобальные переменные
        for g_var in self.global_vars:
            wat += f'  (global ${g_var} (mut f64) (f64.const 0.0))\n'

        # --- 4. Standard Library (Helpers) ---
        wat += self._get_std_lib()

        # --- 5. User Functions ---
        for func_wat in self.funcs_code:
            wat += func_wat + '\n'

        # --- 6. Main Function ---
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
        """
        Встроенные функции для работы с памятью (CONS, CAR, CDR).
        """
        return """
  ;; --- CONS: Выделяет 16 байт, сохраняет car/cdr, возвращает адрес ---
  (func $std_cons (param $car f64) (param $cdr f64) (result f64)
    (local $addr i32)

    ;; $addr = $heap_ptr
    global.get $heap_ptr
    local.tee $addr

    ;; $heap_ptr += 16
    i32.const 16
    i32.add
    global.set $heap_ptr

    ;; store car at $addr
    local.get $addr
    local.get $car
    f64.store

    ;; store cdr at $addr + 8
    local.get $addr
    i32.const 8
    i32.add
    local.get $cdr
    f64.store

    ;; return $addr as f64
    local.get $addr
    f64.convert_i32_u
  )

  ;; --- CAR: Читает f64 по адресу ---
  (func $std_car (param $ptr f64) (result f64)
    local.get $ptr
    i32.trunc_f64_u
    f64.load
  )

  ;; --- CDR: Читает f64 по адресу + 8 ---
  (func $std_cdr (param $ptr f64) (result f64)
    local.get $ptr
    i32.trunc_f64_u
    i32.const 8
    i32.add
    f64.load
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
        if not func_name:
            raise NotImplementedError("Indirect calls not supported")

        args_code = [arg.accept(self) for arg in node.args]
        return "\n    ".join(args_code) + f"\n    call ${func_name}"

    def visit_symbol(self, node: SymbolNode) -> str:
        info = self.current_env.resolve(node.name)
        if not info:
            raise RuntimeError(f"Символ не найден: {node.name}")

        if info.wasm_loc_idx is not None:
            return f"local.get {info.wasm_loc_idx}"
        elif not info.is_function:
            if node.name not in self.global_vars:
                self.global_vars.add(node.name)
            return f"global.get ${node.name}"
        else:
            raise RuntimeError(f"Cannot use function name as variable: {node.name}")

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

    def visit_prim_call(self, node: PrimCallNode) -> str:
        # Базовая математика и сравнения
        op_map = {
            '+': 'f64.add', '-': 'f64.sub', '*': 'f64.mul', '/': 'f64.div',
            '=': 'f64.eq', '<': 'f64.lt', '>': 'f64.gt', '<=': 'f64.le', '>=': 'f64.ge'
        }

        # --- Memory Primitives ---
        if node.prim_name == 'cons':
            if len(node.args) != 2: raise RuntimeError("cons takes 2 args")
            return f"{node.args[0].accept(self)}\n    {node.args[1].accept(self)}\n    call $std_cons"

        if node.prim_name == 'car':
            if len(node.args) != 1: raise RuntimeError("car takes 1 arg")
            return f"{node.args[0].accept(self)}\n    call $std_car"

        if node.prim_name == 'cdr':
            if len(node.args) != 1: raise RuntimeError("cdr takes 1 arg")
            return f"{node.args[0].accept(self)}\n    call $std_cdr"

        # --- Misc ---
        if node.prim_name == 'print':
            return f"{node.args[0].accept(self)}\n    call $print_number\n    f64.const 0.0"

        if node.prim_name == 'not':
            return f"{node.args[0].accept(self)}\n    f64.const 0.0\n    f64.eq\n    f64.convert_i32_s"

        if node.prim_name not in op_map:
            raise NotImplementedError(f"Primitive '{node.prim_name}' not supported")

        # Генерация кода для математики
        args_code = "\n    ".join([arg.accept(self) for arg in node.args])
        code = f"{args_code}\n    {op_map[node.prim_name]}"

        # Конвертация bool (i32) -> f64
        if node.prim_name in ['=', '<', '>', '<=', '>=']:
            code += "\n    f64.convert_i32_s"

        return code

    def visit_cond(self, node: CondNode) -> str:
        if not node.clauses:
            return "f64.const 0.0"

        pred_node, body_nodes = node.clauses[0]

        # Оптимизация для 't'
        if isinstance(pred_node, TrueNode) or (isinstance(pred_node, SymbolNode) and pred_node.name == 't'):
            return self._compile_body(body_nodes)

        code = pred_node.accept(self)
        code += "\n    f64.const 0.0\n    f64.ne"  # check != 0

        code += "\n    (if (result f64)\n      (then\n        " + self._compile_body(body_nodes).replace("\n",
                                                                                                         "\n        ") + "\n      )"

        rest_clauses = node.clauses[1:]
        if rest_clauses:
            code += "\n      (else\n        " + self.visit_cond(CondNode(rest_clauses)).replace("\n",
                                                                                                "\n        ") + "\n      )"
        else:
            code += "\n      (else f64.const 0.0)"

        code += "\n    )"
        return code

    def _compile_body(self, nodes: List[ASTNode]) -> str:
        if not nodes:
            return "f64.const 0.0"
        lines = []
        for i in range(len(nodes) - 1):
            lines.append(nodes[i].accept(self))
            lines.append("    drop")
        lines.append(nodes[-1].accept(self))
        return "\n".join(lines)

    # --- Заглушки ---
    def visit_string(self, node):
        raise NotImplementedError("Strings not supported")

    def visit_quote(self, node):
        raise NotImplementedError("Quote not supported")

    def visit_list(self, node):
        raise NotImplementedError("Raw lists not supported")

    def visit_lambda(self, node):
        raise NotImplementedError("Lambdas not supported")