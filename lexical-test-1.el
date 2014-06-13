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
    (let ((special1 20))
      (setq special 30))
    special1)
  10)) ; this is the tricky one



(ejit/lisp->ejitlisp '(let ((a 20))
    (let ((special1 20))
      (setq special 30))
    special1))
