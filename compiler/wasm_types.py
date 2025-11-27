class WasmType:
    F64 = 'f64'
    I32 = 'i32'
    FUNCREF = 'funcref'


class OpCode:
    # Арифметика
    ADD = 'add'
    SUB = 'sub'
    MUL = 'mul'
    DIV = 'div'

    # Сравнение
    EQ = 'eq'
    NE = 'ne'
    LT = 'lt'
    GT = 'gt'
    LE = 'le'  # <=
    GE = 'ge'  # >=

    # Память
    GET = 'get'  # local.get / global.get
    SET = 'set'  # local.set / global.set
    TEE = 'tee'  # local.tee
    LOAD = 'load'  # f64.load / i32.load
    STORE = 'store'  # f64.store / i32.store
    CONST = 'const'

    # Поток управления
    CALL = 'call'
    CALL_INDIRECT = 'call_indirect'
    DROP = 'drop'
    BR = 'br'
    BR_IF = 'br_if'

    # Преобразование типов
    TRUNC_U = 'trunc_f64_u'  # i32.trunc_f64_u
    CONVERT_U = 'convert_i32_u'  # f64.convert_i32_u


class MemoryLayout:
    WORD_SIZE = 8
    PTR_SIZE = 4
    ENV_PARENT_OFFSET = 0