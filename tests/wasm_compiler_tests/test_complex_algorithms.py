import unittest
from tests.wasm_compiler_tests.base_wasm_test import WasmCompilerTestCase


class TestComplexAlgorithms(WasmCompilerTestCase):
    def test_permutations_count(self):
        """
        Generates permutations for a list of size 3 (1 2 3).

        Includes a dummy string allocation at the start.
        Reason: If the compiler's heap allocator starts at address 0,
        the first cons cell (cons nil nil) might be allocated at 0.
        Since nil is 0.0, the runtime confuses the valid list with nil.
        Allocating a string first bumps the heap pointer, ensuring lists
        have non-zero addresses.
        """
        code = """
        (defun append (l1 l2)
          (if (eq l1 nil) l2 (cons (car l1) (append (cdr l1) l2))))
    
        (defun map (map-f map-lst)
          (if (eq map-lst nil) 
              nil 
              (cons (funcall map-f (car map-lst)) (map map-f (cdr map-lst)))))
    
        (defun flatmap (fm-f fm-lst)
          (if (eq fm-lst nil) 
              nil 
              (append (funcall fm-f (car fm-lst)) (flatmap fm-f (cdr fm-lst)))))
    
        (defun remove (rem-x rem-lst)
          (cond ((eq rem-lst nil) nil)
                ((= rem-x (car rem-lst)) (remove rem-x (cdr rem-lst)))
                (t (cons (car rem-lst) (remove rem-x (cdr rem-lst))))))
    
        (defun range (start end)
          (if (> start end) nil (cons start (range (+ start 1) end))))
    
        (defun perms (p-lst)
          (if (eq p-lst nil)
              (cons nil nil) ;; List containing empty list (( ))
              (flatmap (lambda (x)
                          (map (lambda (p) (cons x p))
                               (perms (remove x p-lst)))) 
                       p-lst)))
    
        (defun length (len-lst)
            (if (eq len-lst nil) 0 (+ 1 (length (cdr len-lst)))))
    
        (defun main ()
            ;; Burn some memory to ensure cons cells aren't allocated at 0
            (setq burn-heap "OFFSET_MEMORY_ALLOCATION") 
            (length (perms (range 1 3))))
    
        (main)
        """
        self.assert_evaluates(code, 6.0)

    def test_dfs_path_existence(self):
        """
        Performs DFS on a graph to find if a path exists.
        Returns 1.0 if path found, 0.0 otherwise.
        """
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
                  (t (if (= 1 (dfs (car neighbors) target visited graph))
                         1
                         (try-neighbors (cdr neighbors) target visited graph)))))

        (defun dfs (current target visited graph)
            (cond ((= current target) 1)
                  ((= 1 (member current visited)) 0)
                  (t (try-neighbors (get-neighbors current graph) 
                                    target 
                                    (cons current visited) 
                                    graph))))

        (defun main ()
             (let ((graph (list (list 1 2 3) (list 2 4) (list 3 4) (list 4 5))))
                  (dfs 1 5 nil graph)))

        (main)
        """
        self.assert_evaluates(code, 1.0)