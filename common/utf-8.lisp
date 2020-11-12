(in-package #:unicode-common)


;; Useing ~ as visual space
(eval-when (:compile-toplevel :load-toplevel :execute)
  (copy-readtable)
  (set-syntax-from-char #\~ #\Space))


(defun code-point-encode-utf-8 (code-point)
  "Returns five values: octet count, octet 0, octet 1, octet 3, octet 4."
  (check-type code-point scalar-value)
  (etypecase code-point

    ;;         code-point range  |     octet |        | octet upper       |  bit |      bit | from code-point
    ;;           (inclusive)     |     count |        | bits              | size | position | or zero padding

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
    ((integer  #x10000 #x10ffff)   (values 4   (logior #b11110000   (ldb (byte  3       18)   code-point))
                                           ~   (logior #b10000000   (ldb (byte  6       12)   code-point))
                                           ~   (logior #b10000000   (ldb (byte  6        6)   code-point))
                                           ~   (logior #b10000000   (ldb (byte  6        0)   code-point))))))


(defmacro code-point-decode-utf-8 (&key
                                     ;; input
                                     index-form ;; where to try decoding
                                     start-form ;; limits for decoding
                                     end-form
                                     ref-form ;; * is the index
                                     ;; output
                                     code-point-place
                                     start-place
                                     end-place
                                     errorp-place
                                     )
  (with-gensyms (ref decode bytes-needed lower-boundary upper-boundary start end octet)
    `(macrolet ((,ref (*)
                  ,ref-form))
       (let ((,bytes-needed        0)
             (,lower-boundary      #x80)
             (,upper-boundary      #xBF)
             (,start               ,start-form)
             (,end                 ,end-form))
         (setf ,start-place        ,index-form
               ,end-place          (1+ ,start-place)
               ,code-point-place   (,ref ,start-place)
               ,errorp-place       nil)
         (block ,decode
           ;; 7-bit?
           (when (<= #x00 ,code-point-place #x7F)
             (return-from ,decode))

           ;; In the middle of subsequence?
           (loop :while (<= #x80 ,code-point-place #xBF)
                 :while (< ,start ,start-place)
                 :repeat 3
                 :do (decf ,start-place)
                     (decf ,end-place)
                     (setf ,code-point-place (,ref ,start-place)))

           ;; Inspect first byte of subsequence
           (typecase ,code-point-place
             ((integer #xC2 #xDF)   (setf   ,bytes-needed 1                                               ))
             ((integer #xE0 #xE0)   (setf   ,bytes-needed 2   ,lower-boundary #xA0                        ))
             ((integer #xE1 #xEC)   (setf   ,bytes-needed 2                         ,upper-boundary #xBF  ))
             ((integer #xED #xED)   (setf   ,bytes-needed 2                         ,upper-boundary #x9F  ))
             ((integer #xEE #xEF)   (setf   ,bytes-needed 2                                               ))
             ((integer #xF0 #xF0)   (setf   ,bytes-needed 3   ,lower-boundary #x90                        ))
             ((integer #xF0 #xF3)   (setf   ,bytes-needed 3                                               ))
             ((integer #xF4 #xF4)   (setf   ,bytes-needed 3                         ,upper-boundary #x8F  ))
             (t                     (setf   ,errorp-place :first-octet-invalid) (return-from ,decode)))

           ;; Mask bits off first byte
           (setf ,code-point-place (ldb (byte (- 6 ,bytes-needed) 0) ,code-point-place))

           ;; Decode subsequence
           (loop :repeat ,bytes-needed :do
             (unless (< ,end-place ,end)
               (setf ,errorp-place :truncated-subsequence)
               (return-from ,decode))
             (let ((,octet (,ref ,end-place)))
               (incf ,end-place)
               (unless (<= ,lower-boundary ,octet ,upper-boundary)
                 (setf ,errorp-place :invalid-trailing-octet)
                 (return-from ,decode))
               (setf ,code-point-place (logior (ash ,code-point-place 6) (ldb (byte 6 0) ,octet))
                     ,lower-boundary #x80
                     ,upper-boundary #xBF))))))))
