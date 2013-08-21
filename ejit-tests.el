;;; tests for ejit

(ert-deftest ejit/tanslate ()
  (should
   (equal
    (ejit/translate '(MULT (PLUS a (car b)) 2))
    "MULT (PLUS (a, car (b)), 2)")))

(ert-deftest ejit-compile ()
  (should
   (equal
    (ejit-compile
     '(flet ((myfunc (a)
              (* 20 a)))
       (let ((a 1)
             (b '(10)))
         (* (+ a (car b)) 2))))
    "(function  (myfunc) { (function  (a,b) { MULT (PLUS (a, car (b)), 2) })(); })();")))


;;; ejit-tests.el ends here
