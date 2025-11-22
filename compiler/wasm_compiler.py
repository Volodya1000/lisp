from typing import List, Set
from semantic.ast_nodes import *
from semantic.symbol_table import Environment


class WasmCompiler(ASTVisitor):
    def __init__(self):
        self.global_env = Environment()
        self._init_primitives()
        self.current_env = self.global_env
        self.funcs_code: List[str] = []
        self.global_vars: Set[str] = set()

        # Для поддержки лямбд и косвенных вызовов
        self.table_entries: List[str] = []  # Имена функций для таблицы: ['$func1', '$lambda_0', ...]
        self.lambda_counter = 0

        self.math_ops = {
            '+': 'f64.add', '-': 'f64.sub', '*': 'f64.mul', '/': 'f64.div'
        }
        self.comp_ops = {
            '=': 'f64.eq', '<': 'f64.lt', '>': 'f64.gt',
            '<=': 'f64.le', '>=': 'f64.ge', '!=': 'f64.ne'
        }

    def _init_primitives(self):
        for name in Environment.PRIMITIVES:
            # Определяем их как функции, чтобы visit_call распознал их как прямой вызов
            self.global_env.define(name, is_function=True)

    def compile(self, nodes: List[ASTNode]) -> str:
        self._scan_definitions(nodes)

        main_nodes: List[ASTNode] = []
        for node in nodes:
            if isinstance(node, DefunNode):
                node.accept(self)
            else:
                main_nodes.append(node)

        # Генерируем тело main, в процессе могут появиться лямбды
        main_body = self._compile_block(main_nodes) if main_nodes else "f64.const 0.0"

        # Сборка модуля
        wat = '(module\n'
        wat += self._get_imports()
        wat += self._get_memory_config()
        wat += self._get_type_definitions()  # Типы для call_indirect
        wat += self._get_table_config()  # Таблица функций
        wat += self._get_globals_definitions()
        wat += self._get_std_lib()

        for func_code in self.funcs_code:
            wat += func_code + '\n'

        wat += '  (func $main (export "main") (result f64)\n'
        wat += self._indent(main_body, 4)
        wat += '\n  )\n'
        wat += ')'
        return wat

    def _get_imports(self) -> str:
        return """
  (import "env" "print_number" (func $print_number (param f64)))
  (import "env" "princ" (func $princ (param f64)))
"""

    def _get_memory_config(self) -> str:
        return '  (memory (export "memory") 1)\n'

    def _get_type_definitions(self) -> str:
        """Генерирует сигнатуры типов для call_indirect (от 0 до 10 аргументов)"""
        types = ""
        for i in range(11):
            params = " ".join(["(param f64)"] * i)
            types += f"  (type $type_{i} (func {params} (result f64)))\n"
        return types

    def _get_table_config(self) -> str:
        """Создает таблицу функций и заполняет её (elem ...)"""
        count = len(self.table_entries)
        if count == 0:
            return "  (table 0 funcref)\n"

        config = f"  (table {count} funcref)\n"
        # Заполняем таблицу именами функций, начиная с индекса 0
        funcs_list = " ".join(self.table_entries)
        config += f"  (elem (i32.const 0) {funcs_list})\n"
        return config

    def _get_globals_definitions(self) -> str:
        code = '  (global $heap_ptr (mut i32) (i32.const 8))\n'
        code += '  (global $scratch (mut f64) (f64.const 0.0))\n'
        for g_var in self.global_vars:
            code += f'  (global ${g_var} (mut f64) (f64.const 0.0))\n'
        return code

    def _get_std_lib(self) -> str:
        # (Вставьте сюда полный код std_lib из предыдущих ответов)
        # Для краткости я приведу только начало, но в файле должен быть весь код
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

    def _scan_definitions(self, nodes: List[ASTNode]):
        for node in nodes:
            if isinstance(node, DefunNode):
                self.global_env.define(node.name, is_function=True)
                # Регистрируем именованные функции в таблице тоже?
                # Пока нет, если нужно передавать defun как значение,
                # нужно будет добавить logic. Для MVP - нет.
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
                raise RuntimeError(f"Compilation failed for node: {node}")
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

    def visit_lambda(self, node: LambdaNode) -> str:
        """
        Компиляция лямбды:
        1. Генерируем уникальное имя для функции.
        2. Компилируем тело функции.
        3. Добавляем функцию в список кода.
        4. Добавляем имя функции в таблицу (Table).
        5. Возвращаем индекс этой функции в таблице как f64.
        """
        name = f"$lambda_{self.lambda_counter}"
        self.lambda_counter += 1

        # Создаем окружение (Примечание: замыкания не поддерживаются,
        # доступ только к аргументам и глобальным переменным)
        func_env = Environment(parent=self.global_env)
        prev_env = self.current_env
        self.current_env = func_env

        for param in node.params:
            self.current_env.define_wasm_local(param)

        body_wat = self._compile_block(node.body)
        self.current_env = prev_env  # Восстанавливаем

        params_wat = " ".join(["(param f64)" for _ in node.params])

        # Создаем WAT код функции
        func_wat = f'  (func {name} {params_wat} (result f64)\n'
        func_wat += self._indent(body_wat, 4) + '\n'
        func_wat += '  )'
        self.funcs_code.append(func_wat)

        # Регистрируем в таблице
        self.table_entries.append(name)
        index = len(self.table_entries) - 1

        return f"f64.const {index}.0"

    def visit_call(self, node: CallNode) -> str:
        # Пытаемся определить, является ли это прямым вызовом известной функции
        is_direct_call = False
        func_name = None

        if isinstance(node.func, SymbolNode):
            sym = self.global_env.resolve(node.func.name)
            # Если символ определен как ФУНКЦИЯ (defun/primitive), то это прямой вызов
            # Если как переменная, то это indirect call
            if sym and sym.is_function:
                is_direct_call = True
                func_name = node.func.name

        if is_direct_call:
            if func_name == 'princ':
                return f"{node.args[0].accept(self)}\ncall $princ\nf64.const 0.0"
            args_code = "\n".join([arg.accept(self) for arg in node.args])
            return f"{args_code}\ncall ${func_name}"
        else:
            # Indirect Call (вызов лямбды или функции из переменной)
            # 1. Компилируем аргументы
            args_code = "\n".join([arg.accept(self) for arg in node.args])

            # 2. Компилируем выражение функции (должно вернуть индекс таблицы)
            func_expr_code = node.func.accept(self)

            # 3. Определяем тип функции по количеству аргументов
            arity = len(node.args)
            type_sig = f"$type_{arity}"

            # 4. Собираем вызов
            # Стек: [arg1, arg2, ..., func_index_f64]
            # Нам нужно: [arg1, arg2, ..., func_index_i32] -> call_indirect
            return f"{args_code}\n{func_expr_code}\ni32.trunc_f64_u\ncall_indirect (type {type_sig})"

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
            # Если имя функции используется как значение (например, передается в другую функцию),
            # мы должны вернуть её индекс в таблице. Но мы пока не регистрируем defun-ы в таблице.
            # Для MVP выбросим ошибку или вернем 0.
            raise RuntimeError(
                f"Function '{node.name}' cannot be used as value (only lambdas supported as values in MVP)")
        else:
            if node.name not in self.global_vars:
                self.global_vars.add(node.name)
            return f"global.get ${node.name}"

    def visit_number(self, node: NumberNode) -> str:
        return f"f64.const {node.value}"

    def visit_string(self, node: StringNode) -> str:
        text_bytes = node.value.encode('utf-8')
        length = len(text_bytes)
        code = "global.get $heap_ptr\n"
        code += f"global.get $heap_ptr\ni32.const {length}\ni32.store\n"
        for i, byte in enumerate(text_bytes):
            code += f"global.get $heap_ptr\ni32.const {4 + i}\ni32.add\ni32.const {byte}\ni32.store8\n"
        aligned_len = 4 + length
        code += f"global.get $heap_ptr\ni32.const {aligned_len}\ni32.add\nglobal.set $heap_ptr\n"
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
            return self.visit_list(node)
        else:
            return "f64.const 0.0"

    def visit_list(self, node: ListNode) -> str:
        if not node.elements:
            return "f64.const 0.0"
        code = "f64.const 0.0"
        for elem in reversed(node.elements):
            elem_code = self._compile_quoted_data(elem)
            code = f"{elem_code}\n{code}\ncall $std_cons"
        return code

    def visit_prim_call(self, node: PrimCallNode) -> str:
        if node.prim_name in self.math_ops:
            args = "\n".join([arg.accept(self) for arg in node.args])
            return f"{args}\n{self.math_ops[node.prim_name]}"
        elif node.prim_name in self.comp_ops:
            args = "\n".join([arg.accept(self) for arg in node.args])
            return f"{args}\n{self.comp_ops[node.prim_name]}\nf64.convert_i32_s"
        elif node.prim_name == 'not':
            return f"{node.args[0].accept(self)}\nf64.const 0.0\nf64.eq\nf64.convert_i32_s"
        elif node.prim_name == 'length':
            return f"{node.args[0].accept(self)}\ncall $std_length"
        elif node.prim_name == 'str-concat':
            return f"{node.args[0].accept(self)}\n{node.args[1].accept(self)}\ncall $std_str_concat"
        elif node.prim_name in ['cons', 'car', 'cdr', 'equal']:
            args = "\n".join([arg.accept(self) for arg in node.args])
            return f"{args}\ncall $std_{node.prim_name}"
        elif node.prim_name in ['null', 'atom']:
            args = "\n".join([arg.accept(self) for arg in node.args])
            return f"{args}\ncall $std_is_nil"
        elif node.prim_name == 'list':
            code = "f64.const 0.0"
            for arg in reversed(node.args):
                code = f"{arg.accept(self)}\n{code}\ncall $std_cons"
            return code
        elif node.prim_name == 'print':
            return f"{node.args[0].accept(self)}\ncall $print_number\nf64.const 0.0"
        elif node.prim_name == 'princ':
            return f"{node.args[0].accept(self)}\ncall $princ\nf64.const 0.0"
        else:
            raise NotImplementedError(f"Primitive {node.prim_name} not implemented")
    def visit_cond(self, node: CondNode) -> str:
        if not node.clauses:
            return "f64.const 0.0"
        return self._compile_cond_recursive(node.clauses)

    def _compile_cond_recursive(self, clauses: List[tuple]) -> str:
        if not clauses:
            return "f64.const 0.0"
        pred, body = clauses[0]
        if isinstance(pred, TrueNode) or (isinstance(pred, SymbolNode) and pred.name == 't'):
            return self._compile_block(body)
        check = f"{pred.accept(self)}\nf64.const 0.0\nf64.ne"
        then_code = self._compile_block(body)
        else_code = self._compile_cond_recursive(clauses[1:])
        return f"{check}\n(if (result f64)\n(then\n{self._indent(then_code, 2)}\n)\n(else\n{self._indent(else_code, 2)}\n)\n)"

    def visit_progn(self, node: PrognNode) -> str:
        if not node.body:
            return "f64.const 0.0"
        lines = []
        for i, expr in enumerate(node.body):
            lines.append(expr.accept(self))
            if i < len(node.body) - 1:
                lines.append("drop")
        return "\n".join(lines)

    def visit_logic(self, node: LogicNode) -> str:
        if not node.args:
            return "f64.const 1.0" if node.op == 'and' else "f64.const 0.0"
        return self._compile_logic_recursive(node.op, node.args)

    def _compile_logic_recursive(self, op: str, args: List[ASTNode]) -> str:
        current, rest = args[0], args[1:]
        curr_code = current.accept(self)
        if not rest:
            return curr_code
        rest_code = self._compile_logic_recursive(op, rest)
        if op == 'and':
            return f"{curr_code}\nf64.const 0.0\nf64.ne\n(if (result f64) (then {self._indent(rest_code, 4)}) (else f64.const 0.0))"
        elif op == 'or':
            return f"{curr_code}\nglobal.set $scratch\nglobal.get $scratch\nf64.const 0.0\nf64.ne\n(if (result f64) (then global.get $scratch) (else {self._indent(rest_code, 4)}))"