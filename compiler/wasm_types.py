class WasmType:
    F64 = 'f64'
    I32 = 'i32'
    FUNCREF = 'funcref'


class OpCode:
    # Math & Logic
    ADD = 'add'
    SUB = 'sub'
    MUL = 'mul'
    DIV = 'div'
    EQ = 'eq'
    NE = 'ne'
    LT = 'lt'
    GT = 'gt'
    LE = 'le'
    GE = 'ge'

    # Memory & Locals
    GET = 'get'  # local.get / global.get
    SET = 'set'  # local.set / global.set
    TEE = 'tee'
    LOAD = 'load'
    STORE = 'store'
    CONST = 'const'

    # Control Flow
    CALL = 'call'
    CALL_INDIRECT = 'call_indirect'
    DROP = 'drop'
    IF = 'if'
    ELSE = 'else'
    END = 'end'
    BR = 'br'
    BR_IF = 'br_if'

    # Conversion
    TRUNC_U = 'trunc_f64_u'
    CONVERT_U = 'convert_i32_u'