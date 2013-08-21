;;; tests for ejit

(ejit->js '(MULT (PLUS a (car b)) 2))

(ejit->js
   (ejit-compile
    '(flet ((myfunc (a)
             (* 20 a)))
      (let ((a 1)
            (b '(10)))
        (* (+ a (car b)) 2)))))   


;;; ejit-tests.el ends here
