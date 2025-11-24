;; == = УТИЛИТЫ == =

;; Проверка вхождения элемента в список
(defun member (x lst)
  (cond
    ((nil lst) nil)
    ((= x (car lst)) t)
    (t (member x (cdr lst)))))

;; Получение соседей узла из графа (Assoc List)
;; Формат графа: ((1 2 3) (2 4) (3 4) (4 5) (5))
;; Ищет список, начинающийся с node
(defun get-neighbors (node graph)
  (cond
    ((nil graph) nil)
    ((= node (car (car graph))) (cdr (car graph)))
    (t (get-neighbors node (cdr graph)))))

;; Реверс списка (для красивого вывода пути)
(defun reverse-helper (lst acc)
  (cond
    ((nil lst) acc)
    (t (reverse-helper (cdr lst) (cons (car lst) acc)))))

(defun reverse (lst)
  (reverse-helper lst nil))

;; == = DFS (ПОИСК В ГЛУБИНУ) == =

;; Основная функция поиска
;; Возвращает список узлов (путь) или nil, если пути нет
(defun dfs (current target visited graph)
  (cond
    ;; 1. Если пришли к цели - возвращаем список из одной цели
    ((= current target) (list target))
    ;; 2. Если уже были здесь - тупик (цикл), возвращаем nil
    ((member current visited) nil)
    ;; 3. Иначе ищем среди соседей
    (t
      ;; Создаем "локальную" переменную path через вызов лямбды
      (funcall (lambda ()
                 (let ((path (try-neighbors (get-neighbors current graph) target (cons current visited) graph)))
                   (if (not (nil path))
                       (cons current path)
                       nil)))))))

;; Перебор соседей. Пытается найти путь через первого соседа.
;; Если возвращает nil, пробует следующего.
(defun try-neighbors (neighbors target visited graph)
  (cond
    ((nil neighbors) nil) ;; Соседи кончились, пути нет
    (t
     ;; Пробуем путь через первого соседа
     (let ((res (dfs (car neighbors) target visited graph)))
       (if (not (nil res))
           res
           (try-neighbors (cdr neighbors) target visited graph)))))))

;; == = UI == =

(defun print-path (p)
  (cond
    ((nil p) (princ "\n"))
    (t
     (progn
       (princ (car p))
       (if (not (nil (cdr p))) (princ " -> "))
       (print-path (cdr p))))))

(defun main ()
  (progn
    ;; Определение графа:
    ;; 1 -> 2, 3
    ;; 2 -> 4
    ;; 3 -> 4, 5
    ;; 4 -> 2 (цикл!), 6
    ;; 5 -> 6
    ;; 6 -> (нет выхода)
    (setq graph
          (list
           (list 1 2 3)
           (list 2 4)
           (list 3 4 5)
           (list 4 2 6)
           (list 5 6)
           (list 6)))

    (princ "Defined Graph Edges:\n")
    (princ "1 -> [2, 3]\n2 -> [4]\n3 -> [4, 5]\n4 -> [2, 6]\n5 -> [6]()
