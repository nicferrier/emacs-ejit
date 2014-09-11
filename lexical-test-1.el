;;; -*- lexical-binding: t -*-

;; some code to illustrate little elisp gotchas

(defvar special1 10)

(should
 (equal
  (let ((a 20))
    special1)
  10)) ; of course

(should
 (equal
  (let ((a 20)
        (special1 20))
    special1)
  20)) ; same here

(should
 (equal
  (let ((a 20))
    (let ((special1 20)) ; overrides the special
      (setq special 30))
    special1) ; the override is no longer in force so reverts to the special
  10)) ; this is the tricky one


(setq tricky-form
      '(let ((a 20))
        (let ((special1 20))
          (setq special 30))
        special1))

(-filter 'symbolp (-flatten tricky-form))

(ejit/lisp->ejitlisp tricky-form)

(ejit-compile2 tricky-form)

(progn
  (defun nic-test (a) (setq a 10))
  (nic-test2 20))


(flet ((a (x y z) x)) (a 10))

(ejit-nodejs tricky-form)


(ejit/lisp->ejitlisp
 '(progn
   (defun a (x) x)
   (let ((a 20)
         (special1 20))
     (setq special1 30)
     special1)))

(progn
  (FSET a (FUNCTION (x) x))
  (CALL-FUNC
   (FUNCTION (a special1)
     (progn (SETQ special1 30) special1))
   (list 20 20)))


(ejit/lisp->ejitlisp '(flet ((a (x)
                              x))
                       (a 10)))
(CALL-FUNC
 (FUNCTION
  (vnew)
  (progn
    (CALL-FUNC
     (FUNCTION
      (old)
      (progn
        (CALL-FUNC
         (FUNCTION nil
                   (progn
                     (TRYFINALLY
                      (progn
                        (IF
                         (eq vnew 'cl--unbound)
                         (fmakunbound 'a)
                         (progn
                           (fset 'a vnew)))
                        (a 10))
                      (IF
                       (eq old 'cl--unbound)
                       (fmakunbound 'a)
                       (progn
                         (fset 'a old))))))
         (list))))
     (list
      (IF
       (fboundp 'a)
       (symbol-function 'a)
       (progn 'cl--unbound))))))
 (list
  (FUNCTION
   (x)
   (progn x))))















(setq tricky2 
      '(progn
        (message
         "%s"
         (funcall
          (lambda ()
            (let ((y 1))
              y))))
        ))

(ejit/lisp->ejitlisp tricky2)
(ejit-nodejs tricky2 2)
(progn
  (message "%s"
           (CALL-FUNC
            (FUNCTION nil
                      (progn
                        (CALL-FUNC
                         (FUNCTION
                          (y)
                          (progn y))
                         (list 1))))
            nil)))







(let ((f (cl-function (lambda (a) 17))))
  (f 7))

(cl-flet ((x (a b c) a))
  (x 10))

(macroexp-unprogn tricky-form)


