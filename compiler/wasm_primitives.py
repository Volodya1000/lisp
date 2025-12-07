from typing import List

class PrimitiveHandler:
    def __init__(self):
        self.math_ops = {
            '+': 'f64.add', '-': 'f64.sub', '*': 'f64.mul', '/': 'f64.div'
        }
        self.comp_ops = {
            '=': 'f64.eq', '<': 'f64.lt', '>': 'f64.gt',
            '<=': 'f64.le', '>=': 'f64.ge', '!=': 'f64.ne',
            'eq': 'f64.eq'
        }
        self.std_mapping = {
            'cons': '$std_cons', 'car': '$std_car', 'cdr': '$std_cdr',
            'equal': '$std_equal', 'length': '$std_length',
            'str-concat': '$std_str_concat', 'nil': '$std_is_nil',
            'atom': '$std_is_nil'
        }

    def handle(self, prim_name: str, args: List, compiler_instance) -> str:
        compiled_args = [arg.accept(compiler_instance) for arg in args]
        args_code = "\n".join(compiled_args)

        if prim_name in self.math_ops:
            return f"{args_code}\n{self.math_ops[prim_name]}"

        if prim_name in self.comp_ops:
            return f"{args_code}\n{self.comp_ops[prim_name]}\nf64.convert_i32_s"

        if prim_name == 'not':
            return f"{compiled_args[0]}\nf64.const 0.0\nf64.eq\n(if (result f64) (then f64.const 1.0) (else f64.const 0.0))"

        if prim_name == 'read':
            return "call $read_num"

        # princ и print должны возвращать значение (0.0), чтобы стек не пустел
        if prim_name == 'princ':
            return f"{compiled_args[0]}\ncall $princ\nf64.const 0.0"

        if prim_name == 'print':
            return f"{compiled_args[0]}\ncall $print_number\nf64.const 0.0"

        if prim_name == 'list':
            code = "f64.const 0.0"
            for arg_code in reversed(compiled_args):
                code = f"{arg_code}\n{code}\ncall $std_cons"
            return code

        if prim_name in self.std_mapping:
            return f"{args_code}\ncall {self.std_mapping[prim_name]}"

        raise NotImplementedError(f"Primitive {prim_name} not implemented")