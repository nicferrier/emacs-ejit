;;; ejit.el - a javascript compiler for emacs-lisp -*- lexical-binding: t -*-

(require 'cl-lib)

(cl-defmacro macroexpand-all-locally (form &environment env)
  "Macroexpand things made with macrolet."
  `(macroexpand-all ,form ',env))

(defvar ejit/trace-log '())

(defun ejit/lisp->ejitlisp (form)
  "Translate Emacs-Lisp to EjitLisp."
  (cl-macrolet 
      ((let (bindings &rest body)
         `(apply
           (lambda ,(mapcar 'car bindings) ,@body)
           (list ,@(mapcar 'cadr bindings))))
       (flet (bindings &rest forms)
         `(let (,@(mapcar
                   (lambda (b)
                     (list (car b) (cons 'lambda (cdr b))))
                   bindings))
            ,@forms))
       (+ (&rest lst) `(PLUS ,@lst))
       (- (&rest lst) `(MINUS ,@lst))
       (* (&rest lst) `(MULT ,@lst))
       (/ (&rest lst) `(DIVIDE ,@lst))
       (function (l-expr) `(FUNCTION ,@(cdr l-expr)))
       (unwind-protect (form handler) `(TRY ,form CATCH ,handler)))
    (macroexpand-all-locally form)))

(defun ejit/print (form)
  "Print FORM as EjitLisp."
  (print (ejit/lisp->ejitlisp form) (current-buffer)))

(defun ejit/translate (ejit-form)
  "Translate EjitLisp to JS."
  (push (list (format "%S" ejit-form)) ejit/trace-log)
  (let ((e (car ejit-form))
        (next (cdr ejit-form)))
    (cond
      ((listp e) (ejit/translate e))
      ((case e
         ('apply (format "(%s)();" (ejit/translate next)))
         ('FUNCTION (cl-destructuring-bind (name defn rest)
                        (if (stringp (car next))
                            (list (car next) (cdr next) '())
                            (list "" next '()))
                      (format "function %s (%s) { %s }"
                              name
                              (mapconcat 'symbol-name (car defn) ",")
                              (ejit/translate (cdr defn)))))))
      ((atom e)
       (format "%s (%s)" e
               (if (not next) ""
                   (mapconcat
                    (lambda (f)
                      (if (atom f)
                          (format "%s" f)
                          (ejit/translate f))) next ", ")))))))

(defun ejit-compile (form &optional insert)
  "Return Javascript for FORM.

Also returns the trace log."
  (interactive
   (list (preceding-sexp)
         current-prefix-arg))
  (let ((ejit/trace-log '()))
    (let* ((js (ejit/translate (ejit/lisp->ejitlisp form)))
           (debug-added (propertize js :trace (copy-list ejit/trace-log))))
      (when insert (insert (format "%s" debug-added)))
      debug-added)))


;;; ejit.el ends here


