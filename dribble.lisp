(in-package #:unicode)


(to-utf-32 "hello world…")

(code-point-at "hello world" 1)

(code-point-count "hello")

(to-utf-16 "hello💩")

(code-point-count (to-utf-16 "💩a…💩"))

(sb-ext:octets-to-string (unicode-data (to-utf-8 (to-utf-16 "💩a…💩"))))

(unicode-data (to-utf-16 (string (code-char #x10302))))

(unicode-to-string (to-utf-16 "💩a…💩"))

(code-point-at "h" 0)

(to-utf-8 "💩a…💩")

(sb-ext:octets-to-string (unicode-utf-8-data (to-utf-8 (to-utf-32 (to-utf-16 (to-utf-8 (to-utf-16 "blåbærsyltetøy")))))))

(defun code-point-to-utf-8% (code-point)
  (let ((buffer (make-array 4 :element-type '(unsigned-byte 8))))
    (subseq buffer 0 (code-point-to-utf-8 code-point buffer 0))))

(setf *print-base* 16)
(setf *print-base* 10)

(code-point-to-utf-8% #xA2)
(code-point-to-utf-8% #x20AC)
(code-point-to-utf-8% #x10348)


(to-utf-8 (string (code-char #xD800)))
(to-utf-16 (string (code-char #xD800)))



;; Possible problems:
;; Is a noncharacter, D14
;; 
