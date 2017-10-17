(in-package #:unicode)

(defvar *read-unicode-format* 'utf-16)

(defun unicode-reader (s c n)
  (declare (ignore c))
  (let* ((format (case n
                  (8 'utf-8)
                  (16 'utf-16)
                  (32 'utf-32)
                  ((nil) *read-unicode-format*)
                  (otherwise
                   (error "Invalid unicode format: ~S" n)))))
    (if (eql #\+ (peek-char nil s))
        (let ((*read-base* 16))
          (let ((code-point (read s)))
            (check-type code-point code-point)
            (make-unicode format (utf-32 code-point))))
        (let ((data (read s t nil t)))
          (etypecase data
            (string
             (make-unicode format data))
            (list
             (apply #'make-unicode format data)))))))

(set-dispatch-macro-character #\# #\u 'unicode-reader)


;; Single code point: #U+1F313
;; String syntax:     #u"Hello World"
;; List syntax:       #u(#U+1F313 "Hello World")
;;                    #u16(#U+1F313 "Hello World" #xD83C #xDF13)
;;                    #u16(#U+1F313) = #u16(#xD83C #xDF13)
