;; --- Библиотека функционального программирования ---

;; Генерация списка чисел от MIN до MAX
(defun range (min max)
  (if (> min max)
      nil
      (cons min (range (+ min 1) max))))

;; Map: применяет функцию F к каждому элементу списка
(defun map (f lst)
  (if (nil lst)
      nil
      (cons (funcall f (car lst))
            (map f (cdr lst)))))

;; Filter: возвращает элементы, удовлетворяющие PRED
(defun filter (pred lst)
  (cond
    ((nil lst) nil)
    ((funcall pred (car lst))
     (cons (car lst)
           (filter pred (cdr lst))))
    (t (filter pred (cdr lst)))))

;; Печать списка через `princ` с разделителем "; "
(defun print-list (lst)
  (cond
    ((nil lst) (princ "\n"))
    ((nil (cdr lst)) (princ (car lst)) (princ "\n")) ; последний элемент без ";"
    (t (princ (car lst))
       (princ " ; ")
       (print-list (cdr lst)))))

;; --- Основная программа ---

(defun start ()
  (progn
    (princ "Generating numbers from 1 to 10...\n")
    (setq numbers (range 1 10))
    (print-list numbers)

    (princ "Filtering numbers > 4...\n")
    (setq filtered (filter (lambda (x) (> x 4)) numbers))
    (print-list filtered)

    (princ "Squaring the filtered numbers...\n")
    (setq squared (map (lambda (x) (* x x)) filtered))
    (print-list squared)

    (princ "Done.\n")))

(start)
