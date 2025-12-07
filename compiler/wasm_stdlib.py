from typing import Set

class WasmStdLib:
    @staticmethod
    def get_imports() -> str:
        """
        Возвращает импорты функций из внешней среды.
        Включает функции для печати чисел/символов и чтения ввода.
        """
        return """
  (import "env" "print_number" (func $print_number (param f64)))
  (import "env" "princ" (func $princ (param f64)))
  (import "env" "read_num" (func $read_num (result f64))) 
"""

    @staticmethod
    def get_memory_config() -> str:
        """
        Настраивает память WebAssembly (экспортирует её под именем 'memory').
        """
        return '  (memory (export "memory") 100)\n'

    @staticmethod
    def get_globals(global_vars: Set[str]) -> str:
        """
        Определяет глобальные переменные:
        - $heap_ptr: указатель на свободное место в куче.
        - $scratch: временная переменная.
        - Пользовательские глобальные переменные.
        """
        code = '  (global $heap_ptr (mut i32) (i32.const 8))\n'
        code += '  (global $scratch (mut f64) (f64.const 0.0))\n'
        for g_var in global_vars:
            code += f'  (global ${g_var} (mut f64) (f64.const 0.0))\n'
        return code


    @staticmethod
    def _get_cons_func() -> str:
        """
        Генерирует функцию $std_cons.
        Создает пару (cons-ячейку) в куче: сохраняет car и cdr и сдвигает указатель кучи.
        """
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
"""

    @staticmethod
    def _get_car_func() -> str:
        """
        Генерирует функцию $std_car.
        Извлекает первый элемент (head) из cons-ячейки по указателю.
        """
        return """
  (func $std_car (param $ptr f64) (result f64)
    local.get $ptr
    i32.trunc_f64_u
    f64.load
  )
"""

    @staticmethod
    def _get_cdr_func() -> str:
        """
        Генерирует функцию $std_cdr.
        Извлекает второй элемент (tail) из cons-ячейки (смещение +8 байт).
        """
        return """
  (func $std_cdr (param $ptr f64) (result f64)
    local.get $ptr
    i32.trunc_f64_u
    i32.const 8
    i32.add
    f64.load
  )
"""

    @staticmethod
    def _get_is_nil_func() -> str:
        """
        Генерирует функцию $std_is_nil.
        Проверяет, является ли значение пустым списком (0.0). Возвращает 1.0 (true) или 0.0 (false).
        """
        return """
  (func $std_is_nil (param $x f64) (result f64)
    local.get $x
    f64.const 0.0
    f64.eq
    (if (result f64) (then f64.const 1.0) (else f64.const 0.0))
  )
"""

    @staticmethod
    def _get_equal_func() -> str:
        """
        Генерирует функцию $std_equal.
        Рекурсивно проверяет структурное равенство (deep equality) двух списков или значений.
        """
        return """
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

    @staticmethod
    def _get_length_func() -> str:
        """
        Генерирует функцию $std_length.
        Читает длину строки или массива (хранится как i32 в начале структуры).
        """
        return """
  (func $std_length (param $ptr f64) (result f64)
    local.get $ptr
    i32.trunc_f64_u
    i32.load
    f64.convert_i32_u
  )
"""

    @staticmethod
    def _get_str_concat_func() -> str:
        """
        Генерирует функцию $std_str_concat.
        Создает новую строку в куче, объединяя содержимое двух переданных строк.
        Копирует байты из первой строки, затем из второй.
        """
        return """
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

    @staticmethod
    def get_runtime_funcs() -> str:
        """
        Собирает все функции стандартной библиотеки воедино.
        """
        return (
            WasmStdLib._get_cons_func() +
            WasmStdLib._get_car_func() +
            WasmStdLib._get_cdr_func() +
            WasmStdLib._get_is_nil_func() +
            WasmStdLib._get_equal_func() +
            WasmStdLib._get_length_func() +
            WasmStdLib._get_str_concat_func()
        )