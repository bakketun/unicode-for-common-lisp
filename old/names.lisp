(in-package :unicode)

(defun is-hex-name (name)
  (loop for char across name
        for first = t then nil
        always (if first
                   (eql #\U char)
                   (digit-char-p char 16))))

(defmacro defpackage-unicode-name ()
  `(progn
     ,@(loop for code from 32 below char-code-limit
             for name = (char-name (code-char code))
             for symbol = (unless (is-hex-name name)
                            (intern name :unicode-name))
             when symbol
               collect `(defconstant ,symbol ,code)
               and collect `(export ',symbol :unicode-name))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :unicode-name)
    (make-package :unicode-name)))

(defpackage-unicode-name)

;(delete-package :unicode-name)
