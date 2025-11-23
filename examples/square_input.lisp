(defun square (x)
    (* x x))

(defun main-loop ()
    (princ "Enter a number to square: ")
    (setq val (read))
    (princ "The square is: ")
    (print (square val)))

(main-loop)