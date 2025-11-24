class FramePolicy:
    """
    Layout: [ParentEnvPtr (8b)] [Var0 (8b)] [Var1 (8b)] ...
    """
    WORD_SIZE = 8
    ENV_PTR_OFFSET = 0

    @staticmethod
    def get_offset(var_index: int) -> int:
        # Индекс 0 идет сразу после EnvPtr (смещение 0)
        return FramePolicy.WORD_SIZE + (var_index * FramePolicy.WORD_SIZE)

    @staticmethod
    def calculate_size(vars_count: int) -> int:
        # 1 слот под ParentEnvPtr + переменные
        return (1 + vars_count) * FramePolicy.WORD_SIZE
