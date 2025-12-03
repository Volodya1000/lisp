import unittest
from tests.wasm_compiler_tests.base_test import WasmCompilerTestCase


class TestIntegrationAlgorithms(WasmCompilerTestCase):

    def test_functional_map_sum(self):
        """Map и Reduce на списках"""
        code = """
        (defun map (f lst)
            (if (eq lst nil) nil
                (cons (f (car lst)) (map f (cdr lst)))))

        (defun sum-list (lst)
            (if (eq lst nil) 0
                (+ (car lst) (sum-list (cdr lst)))))

        (defun main ()
            (let ((nums '(1 2 3)))
                 (sum-list (map (lambda (x) (* x 10)) nums))))
        (main)
        """
        self.assert_evaluates(code, 60.0)

    def test_permutations_stress(self):
        """
        Генерация перестановок списка (1 2 3).
        Этот тест создает много списков, проверяя работу аллокатора.
        """
        code = """
        ;; Определяем вспомогательные функции
        (defun append (l1 l2)
          (if (eq l1 nil) l2 (cons (car l1) (append (cdr l1) l2))))

        (defun flatmap (fm-f fm-lst)
          (if (eq fm-lst nil) nil 
              (append (fm-f (car fm-lst)) (flatmap fm-f (cdr fm-lst)))))

        (defun map (map-f map-lst)
          (if (eq map-lst nil) nil 
              (cons (map-f (car map-lst)) (map map-f (cdr map-lst)))))

        (defun remove (rem-x rem-lst)
          (cond ((eq rem-lst nil) nil)
                ((= rem-x (car rem-lst)) (remove rem-x (cdr rem-lst)))
                (t (cons (car rem-lst) (remove rem-x (cdr rem-lst))))))

        (defun perms (p-lst)
          (if (eq p-lst nil)
              '(())  ;; Список, содержащий пустой список
              (flatmap (lambda (x)
                          (map (lambda (p) (cons x p))
                               (perms (remove x p-lst)))) 
                       p-lst)))

        (defun len (len-lst)
            (if (eq len-lst nil) 0 (+ 1 (len (cdr len-lst)))))

        (defun main ()
            ;; HACK: Выделяем строку в начале, чтобы сдвинуть указатель кучи
            ;; Если первая cons-пара получит адрес 0, она будет равна nil.
            (setq dummy "allocation") 
            (len (perms '(1 2 3))))

        (main)
        """
        # 3! = 6 перестановок
        self.assert_evaluates(code, 6.0)

    def test_dfs_path_finding(self):
        """Поиск пути в графе """
        code = """
        (defun member (x lst)
            (cond ((eq lst nil) 0)
                  ((= x (car lst)) 1)
                  (t (member x (cdr lst)))))

        (defun get-neighbors (node graph)
             (cond ((eq graph nil) nil)
                   ((= node (car (car graph))) (cdr (car graph)))
                   (t (get-neighbors node (cdr graph)))))

        (defun try-neighbors (neighbors target visited graph)
            (cond ((eq neighbors nil) 0)
                  ((= 1 (dfs (car neighbors) target visited graph)) 1)
                  (t (try-neighbors (cdr neighbors) target visited graph))))

        (defun dfs (current target visited graph)
            (cond ((= current target) 1)
                  ((= 1 (member current visited)) 0)
                  (t (try-neighbors (get-neighbors current graph) 
                                    target 
                                    (cons current visited) 
                                    graph))))

        (defun main ()
             ;; 1->(2 3), 2->(4), 3->(4), 4->(5)
             (let ((graph '((1 2 3) (2 4) (3 4) (4 5))))
                  (dfs 1 5 nil graph)))

        (main)
        """
        self.assert_evaluates(code, 1.0)


if __name__ == '__main__':
    unittest.main()