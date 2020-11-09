(in-package #:unicode-base)

(defclass code-unit-string ()
  ()
  (:documentation "A string of code units encoding Unicode text."))

(defclass utf-8-string (code-unit-string)
  ()
  (:documentation "A code unit string in UTF-8 encoding form."))

(defclass utf-16-string (code-unit-string)
  ()
  (:documentation "A code unit string in UTF-16 encoding form."))

(defclass utf-32-string (code-unit-string)
  ()
  (:documentation "A code unit string in UTF-32 encoding form."))


;; Functions

(defgeneric culength (custring)
  (:documentation "Number of code units in the code-unit-string."))

(defgeneric curef (custring index)
  (:documentation "Like svref, but for code-unit-string."))

(defgeneric code-point-at (custring index)
  (:documentation "Returns the code-point (scalar value really) at index.

Values returned are:

code-point - The code point at location or #xFFFD (REPLACEMENT
CHARACTER) if there was a decoding error.

next-index - The index of the next code-point

start - The start index of the code point. Will be less than index
when it points in the middle of a well-formed code unit sequence.

error - True if there was a decoding error."))


;; cl:string as code-unit-string

(defclass char-code-unit-string (#+string-is-utf-8  utf-8-string
                                 #+string-is-utf-16 utf-16-string
                                 #+string-is-utf-32 utf-32-string)
  ((string :type 'string
           :reader custring))
  (:documentation "A string of chararacter objects as code units."))

(defun cuchar (char-code-unit-string index)
  (char (custring char-code-unit-string) index))

(defmethod curef ((custring char-code-unit-string) index)
  (char-code (cuchar custring index)))




;; Internal mixin helper class

(defclass %with-code-units-vector ()
  ((%code-units :type 'vector)))

(defmethod culength ((custring %with-code-units-vector))
  (length (slot-value custring '%code-units)))

(defmethod curef ((custring %with-code-units-vector) (index fixnum))
  (aref (slot-value custring '%code-units) index))


;; Standard implementation of code-unit-string. Slot names are implementation defined.

(defclass standard-utf-8-string (utf-8-string %with-code-units-vector)
  ((%code-units :type '(vector (unsigned-byte 8)))))

(defclass standard-utf-16-string (utf-16-string %with-code-units-vector)
  ((%code-units :type '(vector (unsigned-byte 16)))))

(defclass standard-utf-32-string (utf-32-string %with-code-units-vector)
  ((%code-units :type '(vector (unsigned-byte 32)))))


;; UTF-32 code-point-at

(defmethod code-point-at ((custring utf-32-string) index)
  (let ((code-point (curef custring index)))
    (typecase code-point
      ;; type                  code-point                next index  start  error
      (scalar-value  (values   code-point                (1+ index)  index  nil))
      (t             (values   +replacement-character+   (1+ index)  index  code-point)))))



;; UTF-16 code-point-at

(defmethod code-point-at ((custring utf-16-string) index)
  (let ((code-point (curef custring index))
        (next (1+ index))
        (start index)
        (error nil))
    (flet ((curef? (index)
             (when (< 0 index (culength custring))
               (curef custring index)))
           (decode (leading trailing)
             (logior (ash  (1- (ldb (byte  4 6) leading)) 16)
                     (ash      (ldb (byte  6 0) leading)  10)
                     (ldb (byte 10 0) trailing))))
      (etypecase code-point
        (scalar-value)
        (high-surrogate  (let ((leading   code-point)
                               (trailing  (curef? (1+ index))))
                           (typecase trailing
                             (low-surrogate (setf code-point (decode leading trailing)
                                                  next (+ 2 index)))
                             (t             (setf code-point +replacement-character+
                                                  error code-point)))))
        (low-surrogate   (let ((leading   (curef? (1- index)))
                               (trailing  code-point))
                           (typecase leading
                             (high-surrogate (setf code-point (decode leading trailing)
                                                   start (1- index)))
                             (t              (setf code-point +replacement-character+
                                                   error code-point)))))))
    (values code-point next start error)))


;; UTF-8 code-point-at

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



;; Defaults are implenetation defined

;; (defvar *default-code-unit-string-format* 'utf-32-string)
;; (defvar *default-utf-8-format* 'standard-utf-8-string)
;; (defvar *default-utf-16-format* 'standard-utf-16-string)
;; (defvar *default-utf-32-format* 'standard-utf-32-string)



