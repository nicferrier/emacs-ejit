;;; elisp has 3 non-local forms:

;; unwind-protect, condition-case, catch/try
;;
;; it would be cool to unify these because js only has one.


(let ((caught
       (catch :exit
         (dolist (x (number-sequence 1 10))
           (when (equal x 7)
             (throw :exit (list :error "random error")))))))
  (if (eq (car-safe caught) :error)
      `(error ,(cdr caught))
      caught))


(defmacro nic-catch-case (var bodyform &rest handlers)
  "`condition-case' attempt with catch."
  (declare (indent 1))
  (let ((errorsym (make-symbol "errorsym"))
        (errorcatch (make-symbol "errorcatch"))
        (errorvar (make-symbol "errorvar")))
    `(cl-macrolet ((nic-error (str)
                              `(throw ,,errorcatch (list ,,errorsym str))))
       (let ((,errorvar 
              (catch (quote ,errorcatch)
                ,bodyform)))
         ;; We must use a gensym'd symbol for error so user code can't make it
         (if (eq (car-safe ,errorvar) (quote ,errorsym))
             (list :we :have :an :error)
             ,errorvar)))))

(nic-catch-case err
  (dolist
      (x (number-sequence 1 10))
    (if (equal x 7)
        (nic-error "wah!")
        (+ x 12))))


