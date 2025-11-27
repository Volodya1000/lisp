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
        (length (perms (range 1 3))))

    (main)