from tests.semantic_tests.base_semantic_test import BaseSemanticTest

class TestSpecificExamples(BaseSemanticTest):
    """Тесты трёх конкретных примеров без ошибок"""

    def test_tf_idf_example(self):
        """Пример с term-freq, doc-freq, idf-smoothed, tf-idf-smoothed"""
        code = """
        (defun term-freq (term doc)
          (let* ((doc-length (length doc))
                 (term-count (count term doc :test #'=)))
            (if (zerop doc-length)
                0.0
                (/ (float term-count) doc-length))))

        (defun doc-freq (term docs)
          (count-if (lambda (d) (member term d :test #'=)) docs))

        (defun idf-smoothed (term docs)
          (let* ((N (length docs))
                 (df (doc-freq term docs)))
            (if (zerop df)
                0.0
                (log (+ 1.0 (/ (float N) df))))))

        (defun tf-idf-smoothed (term doc docs)
          (* (term-freq term doc) (idf-smoothed term docs)))

        (defun run-example-smoothed ()
          (let* ((term 2)
                 (docs '((1 2 3 2)
                         (2 2 5)
                         (1 5 2 2 2))))
            (idf-smoothed term docs)))

        (run-example-smoothed)
        """
        ast = self.parse_and_analyze(code)
        # Проверка: анализ прошёл, AST не пустой
        assert len(ast) > 0

    def test_permutations_example(self):
        """Пример с concat-all, remove-one, permutations и range"""
        code = """
        (defun concat-all (lists)
          (if (null lists) nil
            (append (car lists) (concat-all (cdr lists)))))

        (defun remove-one (x lst)
          (if (null lst) nil
            (if (eql x (car lst))
                (cdr lst)
                (cons (car lst) (remove-one x (cdr lst))))))

        (defun permutations (list)
          (if (null list)
              '(())
              (concat-all
               (mapcar (lambda (x)
                         (mapcar (lambda (p) (cons x p))
                                 (permutations (remove-one x list))))
                       list))))

        (defun range (from to acc)
          (if (> from to) (reverse acc)
            (range (1+ from) to (cons from acc))))

        (permutations (range 1 3 nil))
        """
        ast = self.parse_and_analyze(code)
        assert len(ast) > 0

    def test_dfs_graph_example(self):
        """Пример с DFS графом"""
        code = """
        (defun dfs-graph (graph start-vertex target-vertex)
          (let ((dfs-fn nil))
            (setf dfs-fn
                  (lambda (current-vertex visited-vertices)
                    (block search-path
                      (if (= current-vertex target-vertex)
                          (return-from search-path (list current-vertex)))
                      (let ((neighbor-list (cdr (assoc current-vertex graph))))
                        (dolist (neighbor neighbor-list)
                          (unless (member neighbor visited-vertices)
                            (let ((path (funcall dfs-fn neighbor (cons neighbor visited-vertices))))
                              (when path
                                (return-from search-path (cons current-vertex path)))))))
                      nil)))
            (funcall dfs-fn start-vertex (list start-vertex))))

        (defparameter *graph*
          '((1 . (2 3))
            (2 . (4))
            (3 . (4 5))
            (4 . ())
            (5 . (6))
            (6 . ())))

        (dfs-graph *graph* 1 6)
        """
        ast = self.parse_and_analyze(code)
        assert len(ast) > 0
