(copy-readtable) (set-syntax-from-char #\~ #\Space) ; Using ~ visual space


(defun code-point-utf-8-encode (code-point)
  "Returns five values: octet count, octet 0, octet 1, octet 3, octet 4."
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


(defun utf-8-encode-into (code-points vector &key start end)
  (loop :with pos := (or start 0)
        :with end := (or end (length vector))
        :for code-point :in code-points
        :do (multiple-value-bind (size b0 b1 b2 b3) (code-point-utf-8-encode code-point)
              (assert (<= (+ pos size) end)  () "Not enough room.")
              ~                  (setf (aref vector pos) b0)   (incf pos)
              (when (< 1 size)   (setf (aref vector pos) b1)   (incf pos))
              (when (< 2 size)   (setf (aref vector pos) b2)   (incf pos))
              (when (< 3 size)   (setf (aref vector pos) b3)   (incf pos)))
        :finally (return pos)))


(defun utf-8-length (code-points)
  (loop :for code-point :in code-points
        :summing (code-point-utf-8-encode code-point)))


(defun utf-8-encode (code-points)
  (let ((buffer (make-array (utf-8-length code-points) :element-type '(unsigned-byte 8))))
    (utf-8-encode-into code-points buffer)
    buffer))





(utf-8-encode (map 'list #'char-code "Blåbærsyltetøy"))

(utf-8-encode (map 'list #'char-code "AB"))

(utf-8-encode '(65 66 128 129))

(map 'string #'code-char
     (remove-if-not #'integerp
                    (utf-8-decoder
                     (utf-8-encode (map 'list #'char-code "Blåbærsyltetøy")))))


(code-point-utf-8-encode 19)
(code-point-utf-8-encode #x801)
(ignore-errors (code-point-utf-8-encode #x11ffff))
