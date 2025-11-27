from ..wasm_context import CompilerContext
from ..frame_policy import FramePolicy
from ..wat_builder import WatBuilder

class BaseHandler:
    """
    Базовый класс для обработчиков узлов AST.
    Хранит ссылку на глобальный контекст компиляции.
    """
    def __init__(self, ctx: CompilerContext):
        self.ctx = ctx

    def get_builder(self) -> WatBuilder:
        return WatBuilder()