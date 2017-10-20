(in-package #:unicode)

(defun find-code-point (name)
  (char-code (name-char (substitute #\_ #\Space name))))

(defun unicode-reader (s c n)
  (let ((format (case n
                  (8 'utf-8)
                  (16 'utf-16)
                  (32 'utf-32)
                  (0 'string)
                  ((nil) nil)
                  (otherwise
                   (error "Invalid unicode format: ~S" n))))
        (char (peek-char nil s)))
    (cond ((eql #\+ char)
           (let ((*read-base* 16))
             (let ((code-point (read s)))
               (check-type code-point code-point)
               (unicode** (utf-32 code-point) format))))
          ((eql #\" char)
           (unicode** (read s t nil t) format))
          ((eql #\( char)
           (read-char s t nil t)
           (if (char<= #\A (char-upcase (peek-char nil s t nil t)) #\Z)
               (unicode** (utf-32 (find-code-point
                                   (coerce (loop for char = (peek-char nil s)
                                                 until (char= #\) char)
                                                 collect (char-upcase (read-char s t nil t))
                                                 finally (read-char s t nil t))
                                           'string)))
                          format)
               (unicode** (read-delimited-list #\) s t) format)))
          (t
           (error "Invalid unicode syntax: #~@[~A~]~A~A" n c char))))))


(set-dispatch-macro-character #\# #\u 'unicode-reader)


;; Single code point: #U+1F313
;; String syntax:     #u"Hello World"
;; List syntax:       #u(#U+1F313 "Hello World")
;;                    #u16(#U+1F313 "Hello World" #xD83C #xDF13)
;;                    #u16(#U+1F313) = #u16(#xD83C #xDF13)
;; Named char:        #u(GREEK CAPITAL LETTER DELTA)

(get-macro-character #\))

(list #u(  )  )

(list #u(#U+1F313 "Hello World" #\A )  )

(list #8u1882)
(list #uGREEK_CAPITAL_LETTER_DELTA%he world)

(list #uGREEK_CAPITAL_LETTER_DELTA  )

(list #u(#u(GREEK CAPITAL LETTER DELTA) #u(LATIN SMALL LETTER E WITH ACUTE)))

(list #u(#u(GREEK CAPITAL LETTER DELTa) " this is delta"))


(list #u(a))

