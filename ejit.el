;;; ejit.el - a javascript compiler for emacs-lisp -*- lexical-binding: t -*-

(require 'cl-lib)
(require 's)
(require 'noflet)
(require 'nodejs-repl)
(require 'dash)
(require 'pp)

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


;; I wrote with-escape and put it in org-email, it does pretty much
;; what this does, not returning the catch result but only the
;; non-local result.
(defmacro ejit-catch-case (var bodyform &rest handlers)
  "`condition-case' replacement using catch/throw."
  ;; TODO - the signal form and handling error conditions and all that
  (declare (debug (sexp form sexp))
           (indent 1))
  (let ((errorsym (make-symbol "errorsym"))
        (errorcatch (make-symbol "errorcatch"))
        (catch-result (make-symbol "catch-result")))
    `(flet ((error (str)
                (throw (quote ,errorcatch)
                  (list (quote ,errorsym) 'error str))))
       (let ((,catch-result 
              (catch (quote ,errorcatch)
                ,bodyform)))
         ;; We must use a gensym'd symbol for error so user code can't make it
         (if (eq (car-safe ,catch-result) (quote ,errorsym))
             (let ((,var (cdr ,catch-result)))
               (case (cadr ,catch-result)
                 ,@handlers))
             ,catch-result)))))

(defun ejit/lisp->ejitlisp (form)
  "Translate Emacs-Lisp C forms to EjitLisp."
  (let ((pass1 
         (macrolet
             ((condition-case (var form &rest handlers)
                  `(ejit-catch-case
                       ,var ,form
                       ,@handlers)))
           (macroexpand-all-locally
            (-tree-map
             (lambda (e) (if (eq e 'progn) 'PROGN e))
             form)))))
    ;; The second pass is more complete
    (macrolet 
        ((let (bindings &rest body)
           `(CALL-FUN
             (lambda ,(mapcar 'car bindings) ,@body)
             ,@(mapcar 'cadr bindings)))
         (let* (bindings &rest body)
           (if bindings
               `(let (,(car bindings))
                  (let* (,@(cdr bindings))
                    ,@body))
               ;; Else
               `(let () ,@body)))
         (PROGN (&rest body)
                `(lambda () ,@body))
         (funcall (sym &rest args) `(CALL-FUN ,sym ,args))
         (if (test consq &rest alt) `(IF ,test ,consq (PROGN ,@alt)))
         (cond (&rest lst)
               `(if ,(caar lst)
                    (PROGN ,@(cdar lst))
                    ,@(if (cdr lst)
                          `(cond ,@(cdr lst)))))
         (defvar (sym value &optional docstring) ;; just throw docstring away for now
           `(setq ,sym ,value))
         (defalias (sym func)
           `(FSET ,(cadr sym) ,func))
         (lambda (args &rest body)
           `(FUNCTION ,args ,@body))
         (setq (sym val &rest args)  ;; FIXME - because of `args' we should rewrite to partition
           `(SETQ ,sym ,val))
         (+ (&rest lst) `(PLUS ,@lst))
         (- (&rest lst) `(MINUS ,@lst))
         (* (&rest lst) `(MULT ,@lst))
         (/ (&rest lst) `(DIVIDE ,@lst))
         (function (l-expr) `(FUNCTION ,@(cdr l-expr)))
         (unwind-protect (form handler) `(TRYFINALLY ,form ,handler)))
      ;; We need to do several layers of this because some special forms
      ;; don't expand properly
      (macroexpand-all-locally pass1))))



;;; Elisp expansion of ejit-lisp

(defun ejit/ejitlisp->lisp (form)
  "Take EjitLisp and output EmacsLisp.

EjitLisp is just a representation of EmacsLisp with less special
forms.  There is no `let' or `let*', no `progn', no `cond'.  But
the forms it does have are all directly translateable to
EmacsLisp.  And that's what this does."
  (macrolet
      ((CALL-FUN (sym &rest args)
         (if args
             `(funcall ,sym ,@args)
             `(funcall ,sym)))
       (IF (test consq &rest alt)
           `(if ,test
                ,consq
                ,@alt))
       (FUNCTION (args &rest body)`(lambda ,args ,@body))
       (SETQ (sym val)  `(setq ,sym ,val))
       (FSET (sym func) `(fset ,sym ,func))
       (DIVIDE (&rest lst) `(/ ,@lst))
       (MULT (&rest lst) `(* ,@lst))
       (PLUS (&rest lst) `(+ ,@lst))
       (MINUS (&rest lst) `(- ,@lst))
       (TRYFINALLY (form handler)
         `(unwind-protect
               ,form
            ,handler)))
    (macroexpand-all-locally form)))


;;; js translation

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


(defvar ejit/funcs nil)
(defvar ejit/globals nil)

;; FIXME support SETQ as an operation on some ejit.namespace ??
;;
;; we will have to change FSET as well, and CALL-FUNC... they should
;; look up in a functions namespace
;;
;; questions for this -
;;
;; how do we support calling a function stored in the var namespace?
;;
;; how do we shadow the variable namespace with let?
;; could we do it with trycatch restoring values?
;; could we do it at a lower level, by copying the namespace?
(defun ejit/translate (ejit-form)
  "Translate EjitLisp to JS."
  (let ((e (car ejit-form))
        (next (cdr ejit-form)))
    (push (list (format "%S {%s}" ejit-form e)) ejit/trace-log)
    (cond
      ((listp e) (format "(%s)" (ejit/translate e)))
      ((eq e 'CALL-FUN)
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
      ((eq e 'SETQ)
       (let ((name (car next)))
         (push name ejit/globals)
         (format "ejit.vars[\"%s\"] = %s"
                 name
                 (ejit/translate (cdr next)))))
      ((eq e 'TRYFINALLY)
       (format "try { %s } finally (e) { %s}"
               (ejit/translate (car next))
               (ejit/translate (cadr next))))
      ((eq e 'FSET)
       (let ((func-name (car next)))
         (push func-name ejit/funcs)
         (format "ejit.functions.%s = %s"
                 (ejit/symbol->jsname func-name) ; the name
                 (ejit/translate (cadr next)))))
      ((eq e 'FUNCTION)
       (cl-destructuring-bind (name defn rest)
           (if (stringp (car next)) ; lambdas don't have names
               (list (car next) (cdr next) '())
               (list "" next '()))
         (format "function %s(%s) { %s }" name
                 (mapconcat 'symbol-name (car defn) ",")
                 (ejit/expr-map (cdr defn) :func))))
      ((numberp e) (format "%d" e))
      ((atom e)
       (format
        "%s(%s)"
        (format "ejit.functions.%s" (ejit/symbol->jsname e))
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
  (let ((ejit/trace-log '())
        (ejit/globals nil)
        (ejit/funcs nil))
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
      (propertize full-js :globals ejit/globals :funcs ejit/funcs))))

(defun ejit-compile2 (form)
  "Compile the elisp and drop it in a JS buffer."
  (interactive (list (preceding-sexp)))
  (with-current-buffer (get-buffer-create "*js*")
    (erase-buffer)
    (insert (ejit-compile form))
    (js2-mode)
    (switch-to-buffer-other-window (current-buffer)))
  nil)

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



;;; The side by side editor

(defun ejit/read-forms ()
  "Read all the forms from the current buffer."
  ;; TODO - alter so it reads from regions
  (let (all)
    (condition-case err
        (save-excursion
          (goto-char (point-min))
          (let ((form (read (current-buffer))))
            (while form
              (setq all (append (list form) all))
              (setq form (read (current-buffer))))))
      (end-of-file all)
      (invalid-read-syntax (list :error :invalid-read-syntax))
      (error err))))

(defun ejit/pp-sexp (sexp)
  "Make a pp version of SEXP.

  (ejit/pp-sexp '(lambda (a b c) (let ((x 1)) x)))
  =>
\"(lambda
    (a b c)
  (let
      ((x 1))
    x))
\""
  (with-temp-buffer
    (let ((bufname (buffer-name)))
      (pp-display-expression sexp bufname)
      (buffer-substring (point-min) (point-max)))))

(defun ejit/compile-forms (output-func forms)
  "Compile FORMS and map them over OUTPUT-FUNC."
  (->> forms
    (-map 'ejit/lisp->ejitlisp)
    ;;(-map 'ejit/ejitlisp->lisp)
    (-map output-func)))

(defun ejit-compile-buf (&optional buffer out-buffer)
  "Compile the specified BUFFER or the current-buffer.

Returns the output buffer which is either OUT-BUFFER or a buffer
derived from the source buffer."
  ;; TODO - put the output buffer in a derived mode from emacs-lisp
  ;; that will let us further switch to the javascript output or the
  ;; emacs-lisp evaluated output or the javascript evaluated output
  (interactive)
  (let ((buf (or buffer (current-buffer)))
        (out-buf (or out-buffer
                     (let ((buf
                            (get-buffer-create
                             (format "*ejit/compile-%s*" (buffer-name)))))
                       (prog1 buf
                         (with-current-buffer buf
                           (emacs-lisp-mode)
                           (setq buffer-read-only t)))))))
    (with-current-buffer buf
      (let ((compiled (ejit/read-forms)))
        (with-current-buffer out-buf
          (let ((buffer-read-only nil))
            (erase-buffer)
            (goto-char (point-min))
            (if (and (eq (car-safe compiled) :error)
                     (eq (car-safe (cdr-safe compiled)) :invalid-read-syntax))
                (print ";; parser error" (current-buffer))
                ;; Else if was ok so print i
                (condition-case err
                    (ejit/compile-forms
                     (lambda (form)
                       (let ((str-form (ejit/pp-sexp form)))
                         (goto-char (point-max))
                         (insert str-form "\n")))
                     (reverse compiled))
                  (error (princ (format ";; translate error %s" err) (current-buffer))))))
          (current-buffer))))))

(defun ejit/on-change (&rest args)
  "ARGS is the change function integers which we ignore."
  (ejit-compile-buf))

(defun ejit-pop-compile ()
  (interactive)
  (when (eq major-mode 'emacs-lisp-mode)
    (unless (memq 'ejit-compile-buf after-change-functions)
      (add-hook 'after-change-functions 'ejit/on-change nil t))
    (let ((window (frame-selected-window)))
      (pop-to-buffer (ejit-compile-buf))
      (select-window window))))

;; this is probably badly named - there needs to be a mode for
;; ejit-lisp with TRY-FINALLY syntax coloured and all that... the mode
;; defined here is like interactive-lisp-mode... it's adds stuff to
;; emacs-lisp-mode.
(define-minor-mode ejit-lisp-mode
    "Allow the current Lisp file to be compiled to ejit.

C-c C-z pops up an ejit compile window."
  :keymap (let ((ejit-map (make-sparse-keymap)))
            (define-key ejit-map (kbd "C-c C-z") 'ejit-pop-compile)
            ejit-map))


;;; Evaluating the javascript

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

A promise function is returned.  `funcall' the promise function
to wait on the completion of the data.  The promise function
returns whatever the RECEIVER returns.  The default RECEIVER
simply returns what it was passed as a list."
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
                   (expand-file-name "ejit-compiler.js"
                                     (file-name-directory 
                                      (or load-file-name
                                          buffer-file-name
                                          default-directory))))))
      (with-temp-file filename
        (ejit-compile form t)))
    (noflet ((execjs (filename)
               (destructuring-bind (status-code data-lines)
                   (funcall
                    (proc-shell-promise
                     (format "%s %s" nodejs-repl-command filename)))
                 (if (> status-code 0)
                     ;; (when (not debug))
                     (error "bad js: %s" (s-join "\n" data-lines))
                     (car (-filter (lambda (line)
                                     (not (equal "" line))) data-lines))))))
      (if (not debug)
          (unwind-protect
               (execjs filename)
            (delete-file filename))
          ;; Else compile it and open the file in another buffer
          (execjs filename)
          (find-file-other-window filename)))))

(defmacro ejit-process (&rest body)
  "Evaluate BODY as Javascript in NodeJs."
  `(ejit-nodejs (quote ,@body)))

(provide 'ejit)

;;; ejit.el ends here
