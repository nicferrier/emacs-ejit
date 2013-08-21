;;; infix-demo.el  -*- lexical-binding: t -*-

(defun nic-prefix->infix (prefix-expr separators)
  "Converts a prefix expression to infix"
  ;;; this is from http://folk.uio.no/jornv/infpre/infpre.html
  (noflet ((remove-brackets (lst)
             "Reduses lists with just one item to the item itself"
             (do ((result lst (car result)))
                 ((or (not (consp result))
                      (not (null (cdr result)))) result)))
           (insert-between (lst sep)
             (if (or (not (consp lst))
                     (not (rest lst)))
                 lst
                 (cons (first lst)
                       (mapcan
                        (lambda (x) (list sep x))
                        (rest lst))))))
    (let ((in-expr
           (mapcar
            (lambda (x)
              (remove-brackets
               (if (listp x)
                   (nic-prefix->infix x separators)
                   x)))
            prefix-expr)))
      (if (or (not (listp in-expr))
              (not (member (first in-expr) separators)))
          in-expr
          (insert-between (rest in-expr) (first in-expr))))))


;;; infix-demo.el ends here
