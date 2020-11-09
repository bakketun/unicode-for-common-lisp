(in-package #:unicode-base)


(defclass utf-8-string (code-unit-string)
  ()
  (:documentation "A code-unit-string in UTF-8 encoding form."))


(defclass standard-utf-8-string (utf-8-string byte-vector-code-unit-string)
  ((code-units :type '(vector (unsigned-byte 8)))))


(defmethod code-point-at ((custring utf-8-string) index)
  (let ((first-byte (curef custring index)))
    (etypecase first-byte
      ((integer #x00 #x7f) (values first-byte (1+ index) index nil))
      ((integer #x80 #xFF)
       (let ((start index))
         ;; Backtrack if needed
         (loop :while (plusp start)
               :while (<= #x80 first-byte #xBF)
               :repeat 3
               :do (decf start)
                   (setf first-byte (curef custring start)))
         ;; Inspect the first byte
         (multiple-value-bind               (bytes-needed  mask  lower-boundary  upper-boundary)
             (typecase first-byte
               ((integer #xC2 #xDF) (values  1       #b00011111  #x80            #xBF))
               ((integer #xE0 #xE0) (values  2       #b00001111  #xA0            #xBF))
               ((integer #xE1 #xEC) (values  2       #b00001111  #x80            #xBF))
               ((integer #xED #xED) (values  2       #b00001111  #x80            #x9F))
               ((integer #xEE #xEF) (values  2       #b00001111  #x80            #xBF))
               ((integer #xF0 #xF0) (values  3       #b00000111  #x90            #xBF))
               ((integer #xF0 #xF3) (values  3       #b00000111  #x80            #xBF))
               ((integer #xF4 #xF4) (values  3       #b00000111  #x80            #x8F)))
           (let ((code-point nil)
                 (next (1+ index)))
             ;; If first byte was valid, try to parse subsequence at start
             (when bytes-needed
               (setf code-point (logand first-byte mask))
               (loop :with end := (length custring)
                     :repeat bytes-needed
                     :for byte := (if (< next end)
                                      (curef custring next)
                                      0)
                     :unless (<= lower-boundary byte upper-boundary)
                       :do (setf code-point nil)
                           (return)
                     :do (incf next)
                     :do (setf code-point (logior (ash code-point 6) (ldb (byte 6 0) byte))
                               lower-boundary #x80
                               upper-boundary #xBF)))
             (if code-point
                 ;; well-formed subsequence found
                 (values code-point
                         next
                         start
                         nil)
                 ;; ill-formed subsequence found
                 (values +replacement-character+
                         ;; might have backtracked into a preceeding maximal subpart of an ill-formed subsequence
                         (max next (1+ index))
                         index
                         first-byte)))))))))
