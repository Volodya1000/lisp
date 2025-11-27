from .wasm_types import WasmType, OpCode, MemoryLayout
from .wat_builder import WatBuilder

class FramePolicy:
    """
    Отвечает за стратегию размещения переменных в стеке/куче
    и генерацию кода для доступа к ним.
    """

    @staticmethod
    def calculate_frame_size(vars_count: int) -> int:
        # 1 слот под ParentEnvPtr + переменные
        return (1 + vars_count) * MemoryLayout.WORD_SIZE

    @staticmethod
    def get_var_offset(var_index: int) -> int:
        # Индекс 0 идет сразу после EnvPtr (смещение 8)
        return MemoryLayout.WORD_SIZE + (var_index * MemoryLayout.WORD_SIZE)

    @staticmethod
    def emit_var_address(wb: WatBuilder,
                         current_level: int,
                         target_level: int,
                         var_index: int,
                         is_inside_func: bool):
        # 1. Выбор базового указателя
        if is_inside_func:
            wb.emit_get('$new_env_addr')
        else:
            # В main
            wb.emit_get('$env')
            wb.emit(OpCode.TRUNC_U, WasmType.I32)

        # 2. Проход по цепочке окружений (hoisting)
        hops = current_level - target_level
        for _ in range(hops):
            # Загружаем указатель на родительский фрейм (смещение 0)
            wb.emit(OpCode.LOAD, WasmType.F64) # Загружаем как f64
            wb.emit(OpCode.TRUNC_U, WasmType.I32) # Конвертируем в адрес

        # 3. Добавление смещения конкретной переменной
        offset = FramePolicy.get_var_offset(var_index)
        wb.emit_const(offset, WasmType.I32)
        wb.emit(OpCode.ADD, WasmType.I32)