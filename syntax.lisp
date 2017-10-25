(in-package #:unicode)


(defparameter *unicode-text-syntax-readtable* (copy-readtable nil))


(defvar *unicode-text-syntax-current-type* nil)


(defun hex-code-point-reader (stream char)
  (let* ((hex (coerce (loop with function
                            with non-terminatingp
                            for next-char = (peek-char nil stream nil nil t)
                            while next-char
                            do (multiple-value-setq (function non-terminatingp)
                                 (get-macro-character next-char))
                            until (or (char= #\Space next-char)
                                      (char= #\Newline next-char)
                                      (and function (not non-terminatingp)))
                            collect (read-char stream t nil t))
                      'string))
         (code-point (ignore-errors (parse-integer hex :radix 16))))
    (assert (and (plusp (length hex))
                 code-point
                 (char= #\+ (char hex 0)))
            () "Invalid Unicode text syntax: ~A~A" char hex)
    (check-type code-point code-point)
    (code-point code-point :type *unicode-text-syntax-current-type*)))


(set-macro-character #\U 'hex-code-point-reader t *unicode-text-syntax-readtable*)
(set-macro-character #\u 'hex-code-point-reader t *unicode-text-syntax-readtable*)


(defun find-code-point (name)
  (let ((char (name-char (substitute #\_ #\Space name))))
    (unless char
      (error "No character named ~S." name))
    (char-code char)))


(defun named-code-point-reader (stream char)
  (declare (ignore char))
  (code-point
   (find-code-point
    (coerce (loop for next-char = (read-char stream t nil t)
                  until (char= #\} next-char)
                  collect next-char)
            'string))
   :type *unicode-text-syntax-current-type*))


(set-macro-character #\{ 'named-code-point-reader nil *unicode-text-syntax-readtable*)


(defun code-units-reader (stream char)
  (declare (ignore char))
  (assert *unicode-text-syntax-current-type*
          () "Code units syntax only allowed in Unicode text literals with explict type.")
  (let ((code-units (let ((*read-base* 16))
                      (read-delimited-list #\> stream t))))
    (make-text (length code-units) :type *unicode-text-syntax-current-type* :initial-contents code-units)))

(set-macro-character #\< 'code-units-reader nil *unicode-text-syntax-readtable*)
(set-macro-character #\> (get-macro-character #\) nil) nil *unicode-text-syntax-readtable*)


(defun unicode-text-reader (stream char n)
  (let* ((type (case n
                (8 'utf-8)
                (16 'utf-16)
                (32 'utf-32)
                (0 'string)
                ((nil) nil)
                (otherwise
                 (error "Invalid numeric Unicode text type designator: ~S" n))))
         (*unicode-text-syntax-current-type* type)
         (next-char (peek-char t stream t nil t)))
    (case next-char
      (#\(
       (read-char stream t nil t)
       (let ((*readtable* *unicode-text-syntax-readtable*))
         (concatenate-text (read-delimited-list #\) stream t) :type type)))
      ((#\+)
       (hex-code-point-reader stream char))
      ((#\{)
       (read-char stream t nil t)
       (named-code-point-reader stream next-char))
      ((#\<)
       (read-char stream t nil t)
       (let ((*readtable* *unicode-text-syntax-readtable*))
         (code-units-reader stream next-char)))
      ((#\")
       (text (read stream t nil t) :type type))
      (otherwise
       (error "Invalid Unicode text syntax: #~@[~A~]~A~A" n char next-char)))))


(defun enable-unicode-text-syntax ()
  (set-dispatch-macro-character #\# #\u 'unicode-text-reader))
