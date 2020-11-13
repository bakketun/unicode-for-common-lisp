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


(defmacro code-point-decode-utf-8 (&key ;; input forms
                                     (backtrackp       (error ":backtrackp is required"))
                                     (index-form       (error ":index-form is required"))
                                     (start-form       (when backtrackp (error ":start-form is required when backtrackp is true")))
                                     (end-form         (error ":end-form is required"))
                                     (ref-form         (error ":ref-form is required"))
                                     ;; output places
                                     (code-point-place (gensym "code-point-place-") code-point-place-p)
                                     (start-place      (gensym "start-place-")      start-place-p)
                                     (end-place        (gensym "end-place-")        end-place-p)
                                     (errorp-place     (gensym "errorp-place-")     errorp-place-p))
  "index-form, start-form and end-form are evaluated once, in that order.

ref-form is evaluated at macro expansion time with * bound to the index of the code unit to be accessed. Example: `(aref vector ,*)

backtrackp - when true, generates code that will backtrack at most 3 octets if index points in the middle of a subsequence.

index-form - position where decoding starts.

start-form - only used when backtrackp is true. Limits the backtracing to this position.

end-form - limits decoding to before this position.

code-point-place - the decoded code-point is stored here.

start-place - starting position of the subsequence. Always equal to or atmost 3 less than index-form.

end-place - next position after the subsequence.

errorp-place - true if subsequence was ill-formed. code-point is set
U+FFFD Replacement Character, start-place and end-place to the maximal
subpart of the ill-formed subsequence. Ref. Unicode 13.0 page 126.
"
  (with-gensyms (decode bytes-needed lower-boundary upper-boundary index start end octet)
    (flet ((decode-error (code)
             `(progn (setf ,errorp-place ,code
                           ,code-point-place +replacement-character+)
                     (return-from ,decode)))
           (ref (*)
             (eval ref-form)))
      `(let (,@(unless code-point-place-p `(,code-point-place))
             ,@(unless start-place-p      `(,start-place))
             ,@(unless end-place-p        `(,end-place))
             ,@(unless errorp-place-p     `(,errorp-place))
             (,bytes-needed        0)
             (,lower-boundary      #x80)
             (,upper-boundary      #xBF)
             (,index               ,index-form)
             ,@(when start-form
                 `((,start         ,start-form)))
             (,end                 ,end-form))
         (setf ,start-place        ,index
               ,end-place          (1+ ,start-place)
               ,code-point-place   ,(ref start-place)
               ,errorp-place        nil)
         (block ,decode
           ;; 7-bit?
           (when (<= #x00 ,code-point-place #x7F)
             (return-from ,decode))

           ;; In the middle of subsequence?
           ,(when backtrackp
              `(loop :while (<= #x80 ,code-point-place #xBF)
                     :while (< ,start ,start-place)
                     :repeat 3
                     :do (decf ,start-place)
                         (decf ,end-place)
                         (setf ,code-point-place ,(ref start-place))))

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
             (t ,(decode-error :first-octet-invalid)))

           ;; Mask bits off first byte
           (setf ,code-point-place (ldb (byte (- 6 ,bytes-needed) 0) ,code-point-place))

           ;; Decode subsequence
           (loop :repeat ,bytes-needed :do
             (unless (< ,end-place ,end)
               ,(decode-error :truncated-subsequence))
             (let ((,octet ,(ref end-place)))
               (incf ,end-place)
               (unless (<= ,lower-boundary ,octet ,upper-boundary)
                 ,(decode-error :invalid-trailing-octet))
               (setf ,code-point-place (logior (ash ,code-point-place 6) (ldb (byte 6 0) ,octet))
                     ,lower-boundary #x80
                     ,upper-boundary #xBF)))

           ;; Check if we have backtracked too far
           ,(when backtrackp
              `(when (< ,end-place ,end)
                 (setf ,start-place ,index
                       ,end-place (1+ ,start-place))
                 ,(decode-error :first-octet-invalid*)))

           (values))))))
