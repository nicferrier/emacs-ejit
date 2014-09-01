;;; tests for ejit

(require 's)
(require 'ejit)
(require 'ert)

(defmacro ejit/Z (&rest expression)
  "Make the next expression frameless.

The resulting javascript won't be wrapped in any boilerplate JS."
  `(let ((ejit-compile-frame "${__ejit__}"))
     ,@expression))

(ert-deftest ejit/symbol->jsname ()
  "Test symbol name mangling."
  (should (equal (ejit/symbol->jsname 'nic-test) "nic_test"))
  (should (equal (ejit/symbol->jsname 'nic_test-blah) "nic__test_blah"))
  (should (equal (ejit/symbol->jsname 'nic/test-blah) "nic_slash_test_blah")))

(ert-deftest ejit/expr-map ()
  "Test the expression list mapping routine."
  (should (equal (ejit/expr-map '((value) 3) :func) "ejit.value();return 3;"))
  (should (equal (ejit/expr-map '(3) :func) "return 3;"))
  (should (equal (ejit/expr-map '((value) 3) ";") "ejit.value();3"))
  (should (equal (ejit/expr-map '(1 2 3)) "1, 2, 3")))

(defun ejit/scratch ()
  "a bunch of scratchy working out stuff."
  (ejit/lisp->ejitlisp
   '(flet ((myfunc (a)
            (* 20 a)))
     (let ((a 1)
           (b '(10)))
       (* (+ a (car b)) 2))))

  (ejit/translate (ejit/lisp->ejitlisp '(let ((y 10)) y)))
  ;; => ((function (y) y) 10)

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

  (let (ejit/trace-log)
    (print
     (let (ejit/function-space)
       (list
        (ejit/translate (ejit/lisp->ejitlisp '(defun nic-test (a b)  (+ a b))))
        (ejit/translate '(nic-test 10 11)))
       ejit/function-space))
    (current-buffer))

  (let ((ejit/trace-log ()))
    (ejit/translate
     '(apply (FUNCTION (x y) (MULT 1))
       (list (FUNCTION (a) (MULT 2)) (MULT 3 4))))
    (print ejit/trace-log (current-buffer))))

(ert-deftest ejit/tanslate ()
  (should
   (equal
    (ejit/Z (ejit/translate '(MULT(PLUS a (car b)) 2)))
    "ejit.MULT(ejit.PLUS(a, ejit.car(b)), 2)"))
  (should (equal (ejit/Z (ejit/translate '(quote (1 2 3)))) "[1, 2, 3]"))
  (should (equal (ejit/Z (ejit-compile '(quote (1 2 3)))) "[1, 2, 3]"))
  ;; not sure about this rule
  (should (equal (ejit/Z (ejit/translate '(quote a))) "\"a\""))
  (should (equal (ejit/Z (ejit-compile '(quote a))) "\"a\""))
  ;; lists
  (should (equal (ejit/Z (ejit-compile '(list 1 2 3))) "[1, 2, 3]"))
  (should (equal (ejit/Z (ejit-compile '(list 1 (* 2 3) 3)))
                 "[1, ejit.MULT(2, 3), 3]"))
  (should (equal
           (ejit/Z (ejit/translate
                    '(TRYCATCH (PLUS 10 20) (MULT 2 20))))
           "try { ejit.PLUS(10, 20) } catch (e) { ejit.MULT(2, 20)}")))

(defun ejit-= (str1 str2)
  (equal 
   (s-collapse-whitespace str1)
   (s-collapse-whitespace str2)))

(ert-deftest ejit-compile ()
  (should
   (ejit-=
    (ejit/Z
     (ejit-compile
      (quote
       (flet ((myfunc (a)
                (* 20 a)))
         (let ((a 1)
               (b '(10)))
           (* (+ a (car b)) 2))))))
    "(function (myfunc) { 
   return 
   (function (){
      return 
      (function (a,b) { 
         return 
         (function (){
            return ejit.MULT(ejit.PLUS(a, ejit.car(b)), 2);
          })();
       })(1, [10]);
    })(); 
 })(function (a) { 
      return (function (){ return ejit.MULT(20, a); })(); 
    })")))

(defun ejit-test-compile-buffer ()
  "Test the compile-everything-in-a-buffer function."
  (with-current-buffer (get-buffer "buffer-test.el")
    (save-excursion
      (goto-char (point-min))
      (ejit-compile-buffer (current-buffer)))))

(ert-deftest ejit-function-tx ()
  (should
   (equal
    (ejit/lisp->ejitlisp (quote (let ((nictest 10)) (+ 10 12) nictest)))
    '(CALL-FUNC (FUNCTION (nictest) (progn (PLUS 10 12) nictest)) (list 10))))
  (should
   (equal
    (ejit/lisp->ejitlisp (quote (flet ((nictest () 10)) (nictest))))
    '(CALL-FUNC
      (FUNCTION (nictest)
       (progn (nictest)))  ;; not sure whether this should be CALL-FUNC nictest
      (list (FUNCTION nil (progn 10))))))
  (should
   (equal
    (ejit/lisp->ejitlisp (quote (progn (defun nictest () 10)(nictest))))
    '(progn (FSET nictest (FUNCTION nil (progn (progn 10)))) (nictest)))))

(ert-deftest ejit-node-tests ()
  (should (equal "4" (ejit-process (* (car (cons 2 2)) 2))))
  (should (equal "5" (ejit-process (+ (cadr (cons 2 (cons 3 4))) 2))))
  (should (equal "7" (ejit-process (+ (caddr (cons 2 (cons 3 (cons 5 nil)))) 2))))
  (should (equal "10" (ejit-process (progn (defun nic-test () 10) (nic-test)))))
  ;; Testing require
  (ejit/translate
   '(progn
     (require 'test)
     (nictest "hello"))))

;;; ejit-tests.el ends here
