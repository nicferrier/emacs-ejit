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
       (unwind-protect (form handler) `(TRYCATCH ,form ,handler)))
    (macroexpand-all-locally form)))

(defun ejit/print (form)
  "Print FORM as EjitLisp."
  (print (ejit/lisp->ejitlisp form) (current-buffer)))

(defun ejit/expr-map (lst)
  "Make the LST of forms a comma separated JS expression list."
  (mapconcat
   (lambda (f)
     (if (atom f)
         (format "%s" f)
         (ejit/translate f)))
   lst ", "))

(defconst ejit/builtin-functions
  '(car cdr cons MULT DIVIDE PLUS MINUS)
  "List of functions that are built in to Ejit via JS.")

(defun ejit/translate (ejit-form)
  "Translate EjitLisp to JS."
  (let ((e (car ejit-form))
        (next (cdr ejit-form)))
    (push (list (format "%S {%s}" ejit-form e)) ejit/trace-log)
    (cond
      ((listp e) (ejit/translate e))
      ((eq e 'apply)
       (format "(%s)(%s)"
               (ejit/translate (car next))
               (if (cdr next) (ejit/expr-map (cdadr next)) "")))
      ((eq e 'list)
       (format "[%s]" (ejit/expr-map next)))
      ((eq e 'quote)
       (cond
         ((and (listp next)(listp (car next)))
          (format "[%s]" (ejit/expr-map (car next))))
         ((and (listp next)(atom (car next))) ; not convinced about this rule
          (format "\"%s\"" (car next)))
         (t "")))
      ((eq e 'TRYCATCH)
       (format "try { %s } catch (e) { %s}"
               (ejit/translate (car next))
               (ejit/translate (cadr next))))
      ((eq e 'FUNCTION)
       (cl-destructuring-bind (name defn rest)
           (if (stringp (car next))
               (list (car next) (cdr next) '())
               (list "" next '()))
         (format "function %s(%s) { %s }" name
                 (mapconcat 'symbol-name (car defn) ",")
                 (ejit/translate (cdr defn)))))
      ((numberp e) (format "%d" e))
      ((atom e)
       (format "%s(%s)"
               (-if-let (builtin (member e ejit/builtin-functions))
                 (format "ejit.%s" (car builtin))
                 e)
               (if (not next) ""
                   (ejit/expr-map next)))))))

(defvar ejit-compile-frame "var ejit = require('ejit.js');\n"
  "The Javascript that will be used to preceed the compiled js.")

(defun ejit-compile (form &optional insert)
  "Return Javascript for FORM.

Also returns the trace log."
  (interactive
   (list (preceding-sexp)
         current-prefix-arg))
  (let ((ejit/trace-log '()))
    (let* ((js (ejit/translate (ejit/lisp->ejitlisp form)))
           (debug-added (propertize js :trace (copy-list ejit/trace-log)))
           (full-js
            (if ejit-compile-frame
                (concat ejit-compile-frame debug-added)
                debug-added)))
      (when insert (insert (format "%s" full-js)))
      full-js)))



;;; ejit.el ends here


