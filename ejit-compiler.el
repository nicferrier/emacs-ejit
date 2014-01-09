(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'ejit)
(ejit-compile-file (car command-line-args))
