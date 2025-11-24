;; === ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ ===

;; Объединение двух списков
(defun append (l1 l2)
  (cond
    ((nil l1) l2)
    (t (cons (car l1) (append (cdr l1) l2)))))

;; Map: применяет функцию f к списку
(defun map (f lst)
  (cond
    ((nil lst) nil)
    (t (cons (funcall f (car lst)) (map f (cdr lst))))))

;; FlatMap: делает map, а потом склеивает результаты (append)
(defun flatmap (f lst)
  (cond
    ((nil lst) nil)
    (t (append (funcall f (car lst)) (flatmap f (cdr lst))))))

;; Удаление элемента x из списка lst
(defun remove (x lst)
  (cond
    ((nil lst) nil)
    ((= x (car lst)) (remove x (cdr lst))) ;; Удаляем (пропускаем)
    (t (cons (car lst) (remove x (cdr lst))))))

;; Генерация списка [start ... end]
(defun range (start end)
  (cond
    ((> start end) nil)
    (t (cons start (range (+ start 1) end)))))

;; === ОСНОВНАЯ ЛОГИКА ===

;; Генерация перестановок
(defun perms (lst)
  (cond
    ((nil lst) (list nil)) ;; Базовый случай: список (nil)
    (t
     ;; flatmap проходит по каждому элементу x из lst
     (flatmap (lambda (x)
                ;; Внутренняя map берет перестановки хвоста и добавляет x
                (map (lambda (p) (cons x p))
                     (perms (remove x lst))))
              lst))))

(defun print-row (lst)
  (progn
    (princ "[ ")
    (map (lambda (x) (progn (print x) (princ " "))) lst)
    (princ "]\n")))

(defun main ()
  (progn
    (princ "Enter number of elements (N): ")
    (setq n (read))

    (princ "Generating permutations for list of size ")
    (print n)
    (princ "...\n")

    ;; Создаем список (1 2 ... N)
    (setq my-list (range 1 n))

    ;; Вычисляем перестановки
    (setq result (perms my-list))

    ;; Выводим результат
    (map (lambda (row) (print-row row)) result)

    (princ "Done.\n")))

(main)
