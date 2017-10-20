(in-package #:unicode)


(defun find-code-point (name)
  (if (and (<= 4 (length name) 8)
           (loop for char across name always (digit-char-p char 16)))
      (parse-integer name :radix 16)
      (let ((char (name-char (substitute #\_ #\- name))))
        (unless char
          (error "No character named ~S." name))
        (char-code char))))


(defun unicode-reader (s c n)
  (let ((format (case n
                  (8 'utf-8)
                  (16 'utf-16)
                  (32 'utf-32)
                  (0 'string)
                  ((nil) nil)
                  (otherwise
                   (error "Invalid unicode format: ~S" n))))
        (char (read-char s t nil t)))
    (cond ((eql #\+ char)
           (unicode** (utf-32
                       (find-code-point
                        (coerce (loop for char = (peek-char nil s nil nil t)
                                      while (and char
                                                 (or (char= #\- char)
                                                     (char= #\_ char)
                                                     (char<= #\0 char #\9)
                                                     (char<= #\A char #\Z)
                                                     (char<= #\a char #\z)))
                                      collect (char-upcase (read-char s t nil t)))
                                'string)))
                      format))
          ((or (eql #\" char)
               (eql #\( char))
           (unread-char char s)
           (unicode** (read s t nil t) format))
          (t
           (error "Invalid unicode syntax: #~@[~A~]~A~A" n c char)))))


(set-dispatch-macro-character #\# #\u 'unicode-reader)


;; Single code point: #U+1F313
;; Named char:        #U+GREEK_CAPITAL_LETTER_DELTA
;;                    #U+GREEK-CAPITAL-LETTER-DELTA
;; String syntax:     #u"Hello World"
;; List syntax:       #u(#U+1F313 "Hello World")
;;                    #u16(#U+1F313 "Hello World" #xD83C #xDF13)
;;                    #u16(#U+1F313) = #u16(#xD83C #xDF13)
