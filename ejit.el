;;; ejit.el - a javascript compiler for emacs-lisp -*- lexical-binding: t -*-

(require 'cl-lib)
(require 's)
(require 'noflet)
(require 'nodejs-repl)
(require 'dash)

(cl-defmacro macroexpand-all-locally (form &environment env)
  "Macroexpand things made with macrolet."
  `(macroexpand-all ,form ',env))

(defvar ejit/trace-log '())

;; notes
;;
;; the macro compiler could do lisp-2 ness I think
;;
;; defun could be transformed to an fset
;; fset could be further transformed to a setq on a particular namespace

(defun ejit/lisp->ejitlisp (form)
  "Translate Emacs-Lisp to EjitLisp."
  (cl-macrolet 
      ((let (bindings &rest body)
         `(CALL-FUNC
           (lambda ,(mapcar 'car bindings) ,@body)
           (list ,@(mapcar 'cadr bindings))))
       (flet (bindings &rest forms)
         `(let (,@(mapcar
                   (lambda (b)
                     (list (car b) (cons 'lambda (cdr b))))
                   bindings))
            ,@forms))
       (defun (name args &rest body)
           `(FSET ,name (lambda ,args (progn ,@body))))
       (lambda (args &rest body)
         `(FUNCTION ,args (progn ,@body)))
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

(defun ejit/symbol->jsname (symbol)
  "Mangle elisp symbol names so they are acceptable js names."
  (replace-regexp-in-string
   "/" "_slash_"
   (replace-regexp-in-string
    "-" "_"
    (replace-regexp-in-string
     "_" "__" (symbol-name symbol)))))

(defun ejit/expr-map (lst &optional sep)
  "Make the LST of forms a comma separated JS expression list.

If SEP starts with \";\" then the last element of the list of
expressions is morphed into a return statement."
  (noflet ((tx (f)
             (cond
               ((eq f nil) "null")
               ((eq f t) "true")
               ((stringp f)(format "%S" f))
               ((symbolp f) (ejit/symbol->jsname f))
               ((atom f) (format "%s" f))
               ((listp f) (ejit/translate f)))))
    (if (eq sep :func) 
        (destructuring-bind (kar &rest kdr) (reverse (-map 'tx lst))
          (concat
           (mapconcat (lambda (e) (format "%s;" e)) kdr "")
           (format "return %s;" kar)))
        (mapconcat 'tx lst (or sep ", ")))))

(defun ejit/translate (ejit-form)
  "Translate EjitLisp to JS."
  (let ((e (car ejit-form))
        (next (cdr ejit-form)))
    (push (list (format "%S {%s}" ejit-form e)) ejit/trace-log)
    (cond
      ((listp e) (format "(%s)" (ejit/translate e)))
      ((eq e 'CALL-FUNC)
       (format "(%s)(%s)"
               (if (listp (car next))
                   (ejit/translate (car next))
                   (car next))
               (if (cdr next) (ejit/expr-map (cdadr next)) "")))
      ((eq e 'list) (format "[%s]" (ejit/expr-map next)))
      ((eq e 'quote)
       (cond
         ((and (listp next)(listp (car next)))
          (format "[%s]" (ejit/expr-map (car next))))
         ((and (listp next)(atom (car next))) ; not convinced about this rule
          (format "\"%s\"" (car next)))
         (t "")))
      ((eq e 'progn) (format
                      "(function (){ %s })()"
                      (ejit/expr-map next :func)))
      ((eq e 'TRYCATCH)
       (format "try { %s } catch (e) { %s}"
               (ejit/translate (car next))
               (ejit/translate (cadr next))))
      ((eq e 'FSET)
       (let ((func-name (car next)))
         (format "ejit.%s = %s"
                 (ejit/symbol->jsname func-name) ; the name
                 (ejit/translate (cadr next)))))
      ((eq e 'FUNCTION)
       (cl-destructuring-bind (name defn rest)
           (if (stringp (car next))
               (list (car next) (cdr next) '())
               (list "" next '()))
         (format "function %s(%s) { %s }" name
                 (mapconcat 'symbol-name (car defn) ",")
                 (ejit/expr-map (cdr defn) :func))))
      ((numberp e) (format "%d" e))
      ((atom e)
       (format
        "%s(%s)"
        (format "ejit.%s" (ejit/symbol->jsname e))
        (if next (ejit/expr-map next) ""))))))

(defvar ejit-compile-frame "var ejit = require('ejit.js');
console.log(
  ${__ejit__}
);"
  "The Javascript that will be used to wrap the compiled js.

`s-format' is used to produce this, the `s-format' marker
`__ejit__' is replaced with the compiled ejit.")

(defun ejit-compile (form &optional debug)
  "Return Javascript for FORM.

Also returns the trace log."
  (interactive (list (preceding-sexp) current-prefix-arg))
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
      (when debug
        (case debug
          (1 (progn 
               (message "ejit js: %s" js)
               (kill-new js)))
          (2 (progn 
               (message "ejit js: %s" debug-added)
               (kill-new debug-added)))
          (t (insert (format "%s" full-js)))))
      full-js)))

(defun ejit-compile-buffer (buffer &optional debug)
  "Compile the forms in BUFFER to JavaScript.

DEBUG can be specified via the `current-prefix-arg' and has special meanings.

If DEBUG is a buffer then the compiled javascript is inserted
into it and it's made into a js2-mode buffer.  When called
interactively the buffer is constructed if `current-prefix-arg'
is specified."
  (interactive
   (if (eq major-mode 'emacs-lisp-mode)
       (list
        (current-buffer)
        (if (member current-prefix-arg '(1 2))
            current-prefix-arg
            ;; Else make a buffer
            (get-buffer-create
             (concat (file-name-base (buffer-file-name)) ".js"))))))
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
    (when (bufferp debug)
      (with-current-buffer debug
        (erase-buffer)
        (insert js)
        (javascript-mode)
        (switch-to-buffer-other-window (current-buffer))))
    js))

(defun ejit-compile-file (file-name &optional debug)
  "Compile the file of Emacs Lisp to Javascript."
  (interactive
   (list
    (when (eq major-mode 'emacs-lisp-mode) (buffer-file-name))
    current-prefix-arg))
  (let ((out-file (make-temp-file
                   (format "ejit-%s" (file-name-nondirectory file-name))
                   nil "js"))
        (js (ejit-compile-buffer (find-file file-name))))
    (with-temp-file out-file (insert js))
    (when debug (find-file out-file))
    (message out-file)))

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

(defun ejit-nodejs (form &optional debug)
  "Compile FORM to Javascript, save it and run it in NodeJs.

Returns the list of lines that resulted."
  (interactive (list (preceding-sexp) current-prefix-arg))
  (let ((filename (make-temp-file "ejit_node" nil ".js")))
    (let ((ejit-compile-frame
           (format "var ejit = require (process.cwd() + \"/ejit.js\");
ejit.emacs_env = \"node\";
ejit.emacs_process = \"%s\";
ejit.ejit_compiler_location = \"%s\";
console.log(${__ejit__});\n"
                   (concat invocation-directory invocation-name)
                   (concat
                    (file-name-directory 
                     (or load-file-name buffer-file-name))
                    "ejit-compiler.js"))))
      (with-temp-file filename
        (ejit-compile form t)))
    (noflet ((compilejs (filename)
               (destructuring-bind (status-code data-lines)
                   (funcall
                    (proc-shell-promise
                     (format "%s %s" nodejs-repl-command filename)))
                 (if (> status-code 0)
                     (when (not debug)
                       (error "bad js: %s" (s-join "\n" data-lines)))
                     (car (-filter (lambda (line)
                                     (not (equal "" line))) data-lines))))))
      (if (not debug)
          (unwind-protect
               (compilejs filename)
            (delete-file filename))
          ;; Else compile it and open the file in another buffer
          (compilejs filename)
          (find-file-other-window filename)))))

(defmacro ejit-process (&rest body)
  "Evaluate BODY as Javascript in NodeJs."
  `(ejit-nodejs (quote ,@body)))

(provide 'ejit)

;;; ejit.el ends here
