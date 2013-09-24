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
       (defun (name args &rest body)
           `(DEFUNC ,name ,args (progn ,@body)))
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

(defun ejit/expr-map (lst &optional sep)
  "Make the LST of forms a comma separated JS expression list."
  ;; TODO we need something cleverer that can understand how to make
  ;; the last thing a return
  (mapconcat
   (lambda (f)
     (cond
       ((eq f nil) "null")
       ((eq f t) "true")
       ((stringp f)(format "%S" f))
       ((atom f) (format "%s" f))
       ((listp f) (ejit/translate f))))
   lst (or sep ", ")))

(defconst ejit/builtin-functions
  '(car cdr cons cadr caddr cadddr require MULT DIVIDE PLUS MINUS)
  "List of functions that are built in to Ejit via JS.")

(defvar ejit/function-space '()
  "The function space used for tracking defuns.")

(defun ejit/translate (ejit-form)
  "Translate EjitLisp to JS."
  (let ((e (car ejit-form))
        (next (cdr ejit-form)))
    (push (list (format "%S {%s}" ejit-form e)) ejit/trace-log)
    (cond
      ((listp e) (format "(%s)" (ejit/translate e)))
      ((eq e 'apply)
       (format "(%s)(%s)"
               (ejit/translate (car next))
               (if (cdr next) (ejit/expr-map (cdadr next)) "")))
      ((eq e 'list) (format "[%s]" (ejit/expr-map next)))
      ((eq e 'quote)
       (cond
         ((and (listp next)(listp (car next)))
          (format "[%s]" (ejit/expr-map (car next))))
         ((and (listp next)(atom (car next))) ; not convinced about this rule
          (format "\"%s\"" (car next)))
         (t "")))
      ((eq e 'progn) (format "{%s;}" (ejit/expr-map next ";")))
      ((eq e 'TRYCATCH)
       (format "try { %s } catch (e) { %s}"
               (ejit/translate (car next))
               (ejit/translate (cadr next))))
      ((eq e 'DEFUNC)
       (let ((func-name (car next)))
         (push func-name ejit/function-space) ; save the func for later
         (format "ejit.%s = (function (%s) { %s})"
                 func-name ; the name
                 (ejit/expr-map (cadr next))
                 (ejit/expr-map (cddr next) ";"))))
      ((eq e 'FUNCTION)
       (cl-destructuring-bind (name defn rest)
           (if (stringp (car next))
               (list (car next) (cdr next) '())
               (list "" next '()))
         (format "function %s(%s) { %s }" name
                 (mapconcat 'symbol-name (car defn) ",")
                 (ejit/expr-map (cdr defn) ";")
                 ;;(ejit/translate (cdr defn))
                 )))
      ((numberp e) (format "%d" e))
      ((atom e)
       (format
        "%s(%s)"
        (-if-let (builtin (member e ejit/builtin-functions))
          ;; Builtins are upcase converted but otherwise no different
          (format "ejit.%s" (upcase (format "%s" (car builtin))))
          (format "ejit.%s" e))
        (if next (ejit/expr-map next) ""))))))

(defvar ejit-compile-frame "var ejit = require('ejit.js');
console.log(
  ${__ejit__}
);"
  "The Javascript that will be used to wrap the compiled js.

`s-format' is used to produce this, the `s-format' marker
`__ejit__' is replaced with the compiled ejit.")

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
                (s-format
                 ejit-compile-frame
                 'aget
                 (list (cons "__ejit__" debug-added)))
                debug-added)))
      (when insert (insert (format "%s" full-js)))
      full-js)))

(defun ejit-compile-buffer (buffer &optional insert-buffer)
  (interactive
   (if (eq major-mode 'emacs-lisp-mode)
       (list
        (current-buffer)
        (get-buffer-create
         (concat (file-name-base (buffer-file-name)) ".js")))))
  (let* (res
         (js
          (condition-case err
              (save-excursion
                (goto-char (point-min))
                (let ((form (read buffer)))
                  (while form
                    (push
                     (let (ejit-trace-log)
                       (propertize
                        (ejit/translate
                         (ejit/lisp->ejitlisp form))
                       :trace (copy-list ejit/trace-log)))
                     res)
                    (setq form (read buffer)))))
            (end-of-file 
             (if ejit-compile-frame
                 (s-format ejit-compile-frame 'aget
                           (list (cons "__ejit__" (s-join ";\n" res))))
                 (format "%S" res))))))
    (when (called-interactively-p 'any)
      (with-current-buffer insert-buffer
        (erase-buffer)
        (insert js))
      (switch-to-buffer-other-window insert-buffer))
    js))


(defmacro cond-re (expression &rest clauses)
  "Evaluate EXPRESSION and then match regex CLAUSES against it.

Each clause looks like (REGEX BODY ...) where REGEX is matched
against EXPRESSION and, if true, BODY is then evaluated.

The match data is saved around the whole thing and restored."
  (declare (indent 1))
  (let ((expr (make-symbol "exprv"))
        (md (make-symbol "mdv")))
    `(let ((,expr ,expression)
           (,md (match-data)))
       (unwind-protect
            (cond 
              ,@(loop for form in clauses
                   collect `((string-match ,(car form) ,expr)
                             ,@(cdr form))))
         (set-match-data ,md)))))

(defun cond-re-test ()
  "A test for `cond-re'."
  (let ((str "hello nic"))
    (cond-re str
      ("hello \\([a-z]+\\)"
       (message "got %s" (match-string 1 str)))
      ("bye" (message "said godbye")))))

(defun proc-shell-promise (command &optional receiver)
  "Do shell COMMAND calling RECEIVER when it's done.

The RECEIVER is called with the numeric completion status and a
list of lines of the output.  A default RECEIVER is supplied if
none is given.

A promise function is returned.  Call the promise to wait on the
completion of the data.  The promise function returns whatever
the RECEIVER returns.  The default RECEIVER simply returns what
it was passed as a list."
  (let (res
        (rfunc (or receiver (lambda (&rest lst) lst)))
        (proc (start-process-shell-command
               "proc" (generate-new-buffer "*proc*") command)))
    (set-process-sentinel
     proc (lambda (p status)
            (setq res
                  (funcall rfunc
                           (cond-re status
                             ("finished\n" 0)
                             ("exited abnormally with code \\([0-9]+\\)\n"
                              (string-to-int (match-string 1 status))))
                           (split-string
                            (with-current-buffer (process-buffer p)
                              (buffer-string)) "\n")))))
    (lambda (&optional millis)
      (while (not res)
        (accept-process-output proc 0 (or millis 100)))
      res)))

(defun ejit-nodejs (form)
  "Compile FORM to Javascript, save it and run it in NodeJs.

Returns the list of lines that resulted."
  (let ((filename (make-temp-file "ejit_node" nil ".js")))
    (let ((ejit-compile-frame
           (format "var ejit = require (process.cwd() + \"/ejit.js\");
ejit.emacs_process = \"%s\";
console.log((function(){${__ejit__}})());"
                   (concat invocation-directory invocation-name))))
      (with-temp-file filename
        (ejit-compile form t)))
    (unwind-protect
         (destructuring-bind (status-code data-lines)
             (funcall
              (proc-shell-promise
               (format "%s %s" nodejs-repl-command filename)))
           (if (> status-code 0)
               (error "bad js: %s" (s-join "\n" data-lines))
               (car (-filter (lambda (line) (not (equal "" line))) data-lines))))
      (delete-file filename))))

(defmacro ejit-process (&rest body)
  "Evaluate BODY as Javascript in NodeJs."
  `(ejit-nodejs (quote ,@body)))

;;; ejit.el ends here


