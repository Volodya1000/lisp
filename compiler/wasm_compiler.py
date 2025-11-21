from typing import List
from semantic.ast_nodes import ASTNode, NumberNode, PrimCallNode, ASTVisitor
from semantic.symbol_table import Environment


class WasmCompiler(ASTVisitor):
    def __init__(self):
        self.instructions = []  # Буфер для инструкций текущей функции

    def compile(self, nodes: List[ASTNode]) -> str:
        """
        Главная точка входа.
        Превращает список AST узлов (программу) в строку формата WAT.
        """
        # Для MVP считаем, что вся программа - это тело функции main
        # Результат последнего выражения будет возвращен.

        body_code = []
        for node in nodes:
            # Очищаем буфер перед каждым выражением верхнего уровня,
            # если мы хотим, чтобы возвращалось только последнее.
            # Но в Lisp обычно выполняются все по очереди.
            # В WASM стек должен быть чист, кроме результата.
            # Для простоты MVP: компилируем всё подряд, но drop'аем результаты кроме последнего.
            # (Пока пропустим drop, предположим, что у нас одно выражение)

            code_chunk = node.accept(self)
            body_code.append(code_chunk)

        # Собираем WAT модуль
        # Мы экспортируем функцию "main", которая возвращает f64 (число)
        wat = '(module\n'
        wat += '  (func $main (export "main") (result f64)\n'

        # Вставляем сгенерированный код с отступами
        for line in body_code:
            wat += f'    {line}\n'

        wat += '  )\n'
        wat += ')'
        return wat

    # --- Visitor Methods ---

    def visit_number(self, node: NumberNode) -> str:
        # WASM работает с float64 (f64)
        return f"f64.const {node.value}"

    def visit_prim_call(self, node: PrimCallNode) -> str:
        # Lisp: (+ 1 2) -> Polish Notation: 1 2 +
        # WASM (stack machine):
        #   f64.const 1
        #   f64.const 2
        #   f64.add

        op_map = {
            '+': 'f64.add',
            '-': 'f64.sub',
            '*': 'f64.mul',
            '/': 'f64.div'
        }

        if node.prim_name not in op_map:
            raise NotImplementedError(f"Primitive '{node.prim_name}' not supported in WASM yet")

        # 1. Компилируем аргументы (они положат свои значения на стек)
        args_code = []
        for arg in node.args:
            args_code.append(arg.accept(self))

        # 2. Добавляем саму операцию
        args_code.append(op_map[node.prim_name])

        # Соединяем в одну строку через перевод строки
        return "\n    ".join(args_code)

    # --- Заглушки для остальных методов (чтобы не падал абстрактный класс) ---
    def visit_symbol(self, node):
        raise NotImplementedError("Symbols not implemented")

    def visit_string(self, node):
        raise NotImplementedError("Strings not implemented")

    def visit_nil(self, node):
        raise NotImplementedError("Nil not implemented")

    def visit_true(self, node):
        raise NotImplementedError("True not implemented")

    def visit_quote(self, node):
        raise NotImplementedError("Quote not implemented")

    def visit_list(self, node):
        raise NotImplementedError("List not implemented")

    def visit_lambda(self, node):
        raise NotImplementedError("Lambda not implemented")

    def visit_call(self, node):
        raise NotImplementedError("Call not implemented")

    def visit_setq(self, node):
        raise NotImplementedError("Setq not implemented")

    def visit_cond(self, node):
        raise NotImplementedError("Cond not implemented")

    def visit_defun(self, node):
        raise NotImplementedError("Defun not implemented")