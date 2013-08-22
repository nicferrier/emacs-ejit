;;; tests for ejit

(ert-deftest ejit/tanslate ()
  (should
   (equal
    (ejit/translate '(MULT (PLUS a (car b)) 2))
    "MULT (PLUS (a, car (b)), 2)"))
  (should (equal (ejit/translate '(quote (1 2 3))) "[1, 2, 3]"))
  (should (equal (ejit-compile '(quote (1 2 3))) "[1, 2, 3]"))
  ;; not sure about this rule
  (should (equal (ejit/translate '(quote a)) "\"a\""))
  (should (equal (ejit-compile '(quote a)) "\"a\""))
  ;; lists
  (should (equal (ejit-compile '(list 1 2 3)) "[1, 2, 3]"))
  (should (equal (ejit-compile '(list 1 (* 2 3) 3))
                 "[1, MULT(2, 3), 3]"))
  (should (equal
           (ejit/translate
            '(TRYCATCH (PLUS 10 20) (MULT 2 20)))
           "try { PLUS(10, 20) } catch(e) { MULT(2, 20)}")))

(defun ejit/scratch ()
  "a bunch of scratchy working out stuff."
  (ejit/lisp->ejitlisp
   '(flet ((myfunc (a)
            (* 20 a)))
     (let ((a 1)
           (b '(10)))
       (* (+ a (car b)) 2))))

  (let ((ejit/trace-log ()))
    (ejit/translate
     '(apply
       (FUNCTION (myfunc)
        (apply
         (FUNCTION (a b)
                   (MULT (PLUS a (car b)) 2))
         (list 1 (quote (10)))))
       (list (FUNCTION (a) (MULT 20 a)))))
    (print ejit/trace-log (current-buffer)))

  (let ((ejit/trace-log ()))
    (ejit/translate
     '(apply (FUNCTION (x y) (MULT 1))
       (list (FUNCTION (a) (MULT 2)) (MULT 3 4))))
    (print ejit/trace-log (current-buffer))))

(ert-deftest ejit-compile ()
  (should
   (equal
    (ejit-compile
     '(flet ((myfunc (a)
              (* 20 a)))
       (let ((a 1)
             (b '(10)))
         (* (+ a (car b)) 2))))
    "(function (myfunc) { (function (a,b) { MULT(PLUS(a, car(b)), 2) })(1, [10]) })(function (a) { MULT(20, a) })"
    )))

;;; ejit-tests.el ends here
