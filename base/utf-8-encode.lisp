(in-package #:unicode-base)


;; Use ~ as visual space
(eval-when (:compile-toplevel :load-toplevel :execute)
  (copy-readtable)
  (set-syntax-from-char #\~ #\Space))


(defun code-point-utf-8-encode (code-point)
  "Returns five values: octet count, octet 0, octet 1, octet 3, octet 4."
  (check-type code-point scalar-value)
  (etypecase code-point

    ;;         code-point range  |     octet |        | octet upper       |  bit |      bit | from code-point
    ;;           (inclusive)     |     count |        | bits              | size | position | or padding zero

    ((integer     #x00 #x7f)       (values 1                                                  code-point
                                           ~                                                  0
                                           ~                                                  0
                                           ~                                                  0))
    ((integer   #x0080 #x07ff)     (values 2   (logior #b11000000   (ldb (byte  5        6)   code-point))
                                           ~   (logior #b10000000   (ldb (byte  6        0)   code-point))
                                           ~                                                  0
                                           ~                                                  0))
    ((integer   #x0800 #xffff)     (values 3   (logior #b11100000   (ldb (byte  4       12)   code-point))
                                           ~   (logior #b10000000   (ldb (byte  6        6)   code-point))
                                           ~   (logior #b10000000   (ldb (byte  6        0)   code-point))
                                           ~                                                  0))
    ((integer #x100000 #x10ffff)   (values 4   (logior #b11110000   (ldb (byte  3       18)   code-point))
                                           ~   (logior #b10000000   (ldb (byte  6       12)   code-point))
                                           ~   (logior #b10000000   (ldb (byte  6        6)   code-point))
                                           ~   (logior #b10000000   (ldb (byte  6        0)   code-point))))))


(defun code-point-sequence-utf-8-length (sequence)
  (let ((length 0))
    (map 'nil (lambda (code-point)
                (incf length (code-point-utf-8-encode code-point)))
         sequence)
    length))
