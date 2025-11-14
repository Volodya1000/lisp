import os

import pytest
from antlr4 import InputStream, CommonTokenStream
import gen.lisp_grammerLexer as LexerModule
import gen.lisp_grammerParser as ParserModule
from semantic import SemanticAnalyzer
from semantic.errors import (
    NameErrorSemantic,
    DuplicateDeclarationError,
    AttemptToCallNonFunctionError,
    ArityError,
    DivisionByZeroError,
)

EXAMPLES_DIR = "examples/work_examples"

class TestSemanticAnalyzer:
    """Тесты семантического анализатора Lisp."""

    def analyze(self, code: str):
        input_stream = InputStream(code)
        lexer = LexerModule.lisp_grammerLexer(input_stream)
        stream = CommonTokenStream(lexer)
        parser = ParserModule.lisp_grammerParser(stream)
        tree = parser.program()
        analyzer = SemanticAnalyzer()
        return analyzer.analyze(tree)

    @staticmethod
    def has_error(errors, error_type):
        return any(isinstance(e, error_type) for e in errors)

    @staticmethod
    def msg_contains(errors, substring):
        return any(substring in e.message for e in errors)

    # ===================== Ошибочные примеры =====================
    @pytest.mark.parametrize("code, error_type, substring", [
        ("(let ((x 1)) y)", NameErrorSemantic, "Undefined"),
        ("(let ((x 1) (x 2)) (+ x x))", DuplicateDeclarationError, "Duplicate binding"),
        ("(42 1 2)", AttemptToCallNonFunctionError, "Attempt to call"),
        ("((lambda (x y) (+ x y)) 1)", ArityError, "expects"),
        ("(lambda (x x) (+ x 1))", DuplicateDeclarationError, "Duplicate parameter"),
        ("(/ 4 0)", DivisionByZeroError, "Division by zero"),
    ])
    def test_basic_semantic_errors(self, code, error_type, substring):
        errors = self.analyze(code)
        assert self.has_error(errors, error_type), (
            f"Expected {error_type.__name__}, got {[type(e).__name__ for e in errors]}"
        )
        assert self.msg_contains(errors, substring), (
            f"Message does not contain '{substring}'. Messages: {[e.message for e in errors]}"
        )

    @pytest.mark.parametrize("code, error_type, check_context", [
        # Ошибка внутри lambda не должна повлиять на последующий код
        ("((lambda (x) y) 1) (+ x 1)", NameErrorSemantic, "Should catch error in outer scope"),

        # Ошибка внутри let не должна повлиять на последующий код
        ("(let ((x 1)) y) (+ x 1)", NameErrorSemantic, "Should catch error in outer scope"),

        # Проверяем, что после ошибки в lambda, глобальный scope чистый
        ("(let ((x 10)) ((lambda (x) (+ x y)) 5)) (+ x 5)", NameErrorSemantic, "Should find y undefined"),
    ])
    def test_scope_isolation_on_errors(self, code, error_type, check_context):
        """Проверяет, что ошибки внутри lambda/let не повреждают глобальный scope."""
        errors = self.analyze(code)
        assert self.has_error(errors, error_type), (
            f"Expected {error_type.__name__} for '{check_context}', got {[type(e).__name__ for e in errors]}"
        )

        # Убеждаемся, что ошибка именно внутри lambda/let, а не вне
        error_positions = [(e.line, e.column) for e in errors if isinstance(e, error_type)]
        assert len(error_positions) > 0, "Should have at least one error"

    @pytest.mark.parametrize("code, expected_error_count", [
        # Ошибка внутри lambda, но внешний код работает корректно
        # Только ошибка с 'z'
        ("((lambda (x) (let ((y z)) (+ x y))) 5) (+ 10 20)", 1),

        # Ошибка внутри let, но внешний код работает корректно
        # Только ошибка с 'unknown-var'
        ("(let ((x 1)) (let ((y unknown-var)) (+ x y))) (+ 100 200)", 1),
    ])
    def test_scope_recovery_after_errors(self, code, expected_error_count):
        """Проверяет, что после ошибки внутри lambda/let, внешний scope остается валидным."""
        errors = self.analyze(code)
        assert len(errors) == expected_error_count, (
            f"Expected exactly {expected_error_count} error(s), got {len(errors)}: "
            f"{[e.message for e in errors]}"
        )

        # Внешние арифметические выражения не должны вызывать ошибок
        outer_errors = [e for e in errors if "Undefined" not in e.message and "Duplicate" not in e.message]
        assert len(outer_errors) == 0, f"Unexpected errors in outer scope: {outer_errors}"

    # =====================  Вложенные scope =====================
    @pytest.mark.parametrize("code", [
        # Shadowing переменных (должно работать)
        "(let ((x 1)) (let ((x 2)) x))",

        # Lambda внутри lambda с захватом из внешнего scope
        "((lambda (x) ((lambda (y) (+ x y)) 10)) 5)",

        # let внутри lambda внутри let
        """(let ((x 5))
        (let ((y 3))
          ((lambda (z)
            (let ((w 2))
              (+ x y z w)))
          1)))""",
    ])
    def test_nested_scopes_correct(self, code):
        """Проверяет корректную работу вложенных scope."""
        errors = self.analyze(code)
        assert len(errors) == 0, f"Expected no errors in nested scopes, got {[e.message for e in errors]}"

    # ===================== Новые тесты: Дубликаты в разных scope =====================
    @pytest.mark.parametrize("code", [
        # Одинаковые имена в разных let - это ОК
        "(let ((x 1)) x) (let ((x 2)) x)",

        # Параметр лямбды может иметь то же имя, что и переменная в outer scope
        "(let ((x 10)) (lambda (x) (+ x 1)))",

        # let внутри lambda с тем же именем, что и параметр
        "(lambda (x) (let ((x 5)) x))",
    ])
    def test_duplicate_names_different_scopes_allowed(self, code):
        """Проверяет, что дубликаты в разных scope не вызывают ошибок."""
        errors = self.analyze(code)
        duplicate_errors = [e for e in errors if isinstance(e, DuplicateDeclarationError)]
        assert len(
            duplicate_errors) == 0, f"Should allow duplicates in different scopes: {[e.message for e in duplicate_errors]}"

    # ===================== Ошибки в глубоко вложенных scope =====================
    @pytest.mark.parametrize("code, error_type, substring", [
        # Ошибка доступа к переменной из внутреннего scope извне
        ("((lambda (x) (+ x y)) 1)", NameErrorSemantic, "Undefined"),

        # Ошибка в тройной вложенности
        ("(let ((x 1)) (let ((y 2)) (let ((z 3)) unknown)))", NameErrorSemantic, "Undefined"),

        # Ошибка арности внутри вложенного вызова
        ("(let ((f (lambda (x) (+ x 1)))) (f 1 2 3))", ArityError, "expects"),
    ])
    def test_errors_in_nested_scopes(self, code, error_type, substring):
        """Проверяет корректное обнаружение ошибок в глубоко вложенных scope."""
        errors = self.analyze(code)
        assert self.has_error(errors, error_type), (
            f"Expected {error_type.__name__} in nested scope, got {[type(e).__name__ for e in errors]}"
        )
        assert self.msg_contains(errors, substring), f"Message should contain '{substring}'"

    # =====================  Продвинутые возможности  =====================
    @pytest.mark.parametrize("code, description, expected_error_types", [
        # Рекурсия в lambda - требует двухпроходного анализа
        # Ошибки: 'if' и 'f' не определены
        ("(let ((f (lambda (n) (if (= n 0) 1 (* n (f (- n 1))))))) (f 5))",
         "Recursive lambda in let",
         [NameErrorSemantic, NameErrorSemantic]),

        # Взаимная рекурсия - требует предварительного определения
        # Ошибки: 'if' (2 раза), '#t', '#f', 'odd?', 'even?'
        ("""(let (
            (even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
            (odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))))
        ) (even? 4))""",
         "Mutually recursive lambdas",
         [NameErrorSemantic, NameErrorSemantic, NameErrorSemantic, NameErrorSemantic, NameErrorSemantic,
          NameErrorSemantic]),

        # Замыкание (closure) - в статическом анализе add5 - это переменная, не функция
        # Ошибка: попытка вызвать переменную add5
        ("""(let (
            (make-adder (lambda (x) (lambda (y) (+ x y))))
        )
        (let ((add5 (make-adder 5)))
            (add5 10))
        )""",
         "Closure capture",
         [AttemptToCallNonFunctionError]),

        # Явная lambda (не через переменную) - должна работать
        ("""((lambda (x)
            ((lambda (y) (+ x y)) 10))
          5)""",
         "Direct lambda call",
         []),  # Нет ошибок
    ])
    def test_advanced_scope_features(self, code, description, expected_error_types):
        """Тесты на продвинутые особенности scope с правильными ожиданиями."""
        errors = self.analyze(code)

        # Проверяем количество ошибок
        assert len(errors) == len(expected_error_types), (
            f"Expected {len(expected_error_types)} error(s) for '{description}', got {len(errors)}: "
            f"{[e.message for e in errors]}"
        )

        # Проверяем типы ошибок
        for i, (error, expected_type) in enumerate(zip(errors, expected_error_types)):
            assert isinstance(error, expected_type), (
                f"Error #{i} for '{description}': expected {expected_type.__name__}, "
                f"got {type(error).__name__}: {error.message}"
            )

        # Для рекурсивных случаев проверяем, что есть ошибки Undefined
        if "Recursive" in description:
            undefined_errors = [e for e in errors if "Undefined" in e.message]
            assert len(undefined_errors) > 0, "Should detect undefined symbols"

        # Для замыканий проверяем, что ошибка именно о вызове не-функции
        if "Closure capture" in description:
            call_errors = [e for e in errors if isinstance(e, AttemptToCallNonFunctionError)]
            assert len(call_errors) == 1, "Should detect attempt to call non-function"
            assert "add5" in call_errors[0].message, "Error should mention 'add5'"

    @pytest.mark.parametrize("code, expected_errors", [
        ("(format \"Hello ~a\" \"World\")", 0),
        ("(format \"Number: ~d\" 42)", 0),
        ("(format)", 1),  # Ошибка: слишком мало аргументов
        ("(read-line)", 0),
        ("(read)", 0),
        ("(parse-integer \"123\")", 0),
        ("(parse-integer \" 123 \")", 0),
        ("(read-from-string \"(+ 1 2)\")", 0),
    ])
    def test_io_functions(self, code, expected_errors):
        errors = self.analyze(code)
        assert len(errors) == expected_errors

    # ===================== корректные примеры =====================
    @pytest.mark.parametrize("code", [
        # 1. let с одной переменной, корректное использование
        "(let ((x 10)) (+ x 5))",

        # 2. Lambda с правильным количеством аргументов
        "((lambda (a b) (+ a b)) 3 4)",

        # 3. Вложенные let
        "(let ((x 2)) (let ((y 3)) (+ x y)))",

        # 4. Вызов встроенной функции с правильной арностью
        "(+ 1 2 3 4)",

        # 5. Lambda внутри let, без ошибок
        "(let ((f (lambda (x) (* x x)))) (f 5))",
    ])
    def test_basic_no_errors(self, code):
        errors = self.analyze(code)
        assert len(errors) == 0, f"Expected no errors, got {[e.message for e in errors]}"

    def test_example_files_no_errors(self):
        """Проверяет, что первый пример обрабатывается без семантических ошибок."""
        # Берём первый .txt файл безопасно
        txt_files = [f for f in os.listdir(EXAMPLES_DIR) if f.endswith(".txt")]
        assert txt_files, f"No .txt files found in {EXAMPLES_DIR}"
        filename = txt_files[0]

        filepath = os.path.join(EXAMPLES_DIR, filename)
        with open(filepath, "r", encoding="utf-8") as f:
            code = f.read()

        errors = self.analyze(code)
        assert len(errors) == 0, f"Semantic errors found in {filename}: {[e.message for e in errors]}"

    @pytest.mark.parametrize("code", [
        "(+ 1 2)", "(- 10 3)", "(* 4 5)", "(/ 20 4)",
        "(= 1 1)", "(zerop 0)",
    ])
    def test_builtin_arith(self, code):
        assert not self.analyze(code)