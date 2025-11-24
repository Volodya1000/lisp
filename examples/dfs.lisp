;; === УТИЛИТЫ ===

;; Проверка вхождения элемента в список
(defun member (x lst)
  (cond
    ((eq lst nil) nil)
    ((= x (car lst)) t)
    (t (member x (cdr lst)))))


;; Получение соседей узла из графа (Assoc List)
(defun get-neighbors (node graph)
  (cond
    ((eq graph nil) nil)
    ((= node (car (car graph))) (cdr (car graph)))
    (t (get-neighbors node (cdr graph)))))


;; Реверс списка
(defun reverse-helper (lst acc)
  (cond
    ((eq lst nil) acc)
    (t (reverse-helper (cdr lst) (cons (car lst) acc)))))

(defun reverse (lst)
  (reverse-helper lst nil))


;; === DFS ===

(defun try-neighbors (neighbors target visited graph)
  (cond
    ((eq neighbors nil) nil)
    (t
     (let ((res (dfs (car neighbors) target visited graph)))
       (if (not (eq res nil))
           res
           (try-neighbors (cdr neighbors) target visited graph))))))


(defun dfs (current target visited graph)
  (cond
    ((= current target) (list target))
    ((member current visited) nil)
    (t
     (let ((path (try-neighbors (get-neighbors current graph)
                                 target
                                 (cons current visited)
                                 graph)))
       (if (not (eq path nil))
           (cons current path)
           nil)))))


;; === UI ===

(defun print-path (p)
  (cond
    ((eq p nil) (princ "No path found or End of path.\n"))
    (t
     (progn
       (print (car p))
       (if (not (eq (cdr p) nil)) (princ " -> "))
       (print-path (cdr p))))))


(defun main ()
  (progn
    (setq graph
          (list
           (list 1 2 3)
           (list 2 4)
           (list 3 4 5)
           (list 4 2 6)
           (list 5 6)
           (list 6)))

    (princ "Defined Graph Edges:\n")
    (princ "1 -> [2, 3]\n2 -> [4]\n3 -> [4, 5]\n4 -> [2, 6]\n5 -> [6]\n")

    (princ "\n--- DFS Search from 1 to 6 ---\n")
    (let ((result (dfs 1 6 nil graph)))
      (print-path result))

    (princ "\nDone.")
  ))

(main)

