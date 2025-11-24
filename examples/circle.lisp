;; --- Рисование круга в консоли ---

;; Вычисление квадрата числа
(defun square (x) (* x x))

;; Проверка, находится ли точка (x, y) внутри круга радиуса r
;; x^2 + y^2 <= r^2
(defun is-inside (x y r)
  (<= (+ (square x) (square y)) (square r)))

;; Цикл по X (от -R до R) для текущей строки Y
(defun loop-x (x end-x y r)
  (cond
    ((> x end-x) (princ "\n")) ;; Конец строки
    (t (progn
         ;; Немного корректируем пропорции терминала (X * 0.5 для визуальной круглости)
         (cond
           ((is-inside x y r) (princ "**"))
           (t (princ "  ")))
         (loop-x (+ x 1) end-x y r)))))

;; Цикл по Y (от -R до R)
(defun loop-y (y end-y r)
  (cond
    ((> y end-y) t) ;; Конец рисования
    (t (progn
         (loop-x (* r -1) r y r)
         (loop-y (+ y 1) end-y r)))))

(defun main ()
  (progn
    (princ "Drawing a circle with radius 8:\n")
    (loop-y -8 8 8)
    (princ "\nDone.")
  ))

(main)