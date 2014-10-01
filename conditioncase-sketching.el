;;; elisp has 3 non-local forms:

;; unwind-protect, condition-case, catch/try
;;
;; it would be cool to unify these because js only has one.


;; First up the condition-case example - this is what we're trying to reduce

(condition-case err
    (dolist
        (x (number-sequence 1 10) (list 'errorsym x))
      (when (equal x 7)
        (error "wah!")))
  (error (list 10 err)))  ;; => (error "wah!")



;; Here's a solution with catch and using noflet to provide the
;; `error' special form

(require 'noflet)

(defmacro nic-catch-case (var bodyform &rest handlers)
  "`condition-case' attempt with catch."
  (declare (debug (sexp form sexp))
           (indent 1))
  (let ((errorsym (make-symbol "errorsym"))
        (errorcatch (make-symbol "errorcatch"))
        (catch-result (make-symbol "catch-result")))
    `(noflet ((nic-error (str)
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

;; Some example uses - this first one with the error signaled...

(nic-catch-case err
  (mapcar
   (lambda (x)
     (if (equal x 7)
      (nic-error "wah!")
      x))
   (number-sequence 1 10))
  (error (list 10 err))) ;; => (error "wah!")

;; ... and this one with no error

(nic-catch-case err
  (mapcar
   (lambda (x)
     (if (equal x 17)
      (nic-error "wah!")
      x))
   (number-sequence 1 10))
  (error (list 10 err))) ;; => (1 2 3 4 5 6 7 8 9 10)

;; ... and this one sending the same symbol as the error but it
;; doesn't error..

(nic-catch-case err
  (list 'errorsym 'error "an error")
  (error (list 10 err))) ;; =>  (errorsym error "an error")

;; so this proves that the hygiene works because we don't cause an
;; error by using the same error signalling mechanism.
