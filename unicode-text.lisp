(in-package #:unicode)


;; Types

(deftype code-point ()
  '(integer #x0 #x10FFFF))


(deftype unicode-scalar ()
  '(or (integer 0 #xD7FF)
    (integer #xE000 #x10FFFF)))


(deftype utf-8-code-unit () '(unsigned-byte 8))
(deftype utf-16-code-unit () '(unsigned-byte 16))
(deftype utf-32-code-unit () '(unsigned-byte 32))
(deftype string-code-unit () `(integer 0 ,(1- char-code-limit)))


(defgeneric utf-8-p (thing) (:method (thing)))
(deftype utf-8 () '(satisfies utf-8-p))


(defgeneric utf-16-p (thing) (:method (thing)))
(deftype utf-16 () '(satisfies utf-16-p))


(defgeneric utf-32-p (thing) (:method (thing)))
(deftype utf-32 () '(satisfies utf-32-p))


(defun unicodep (object)
  (or (utf-8-p object)
      (utf-16-p object)
      (utf-32-p object)))
(deftype unicode () '(satisfies unicodep))


(defconstant +string-unicode-type+
  #+string-is-utf-8 'utf-8
  #+string-is-utf-16 'utf-16
  #+string-is-utf-32 'utf-32)


(defun unicode-type (unicode)
  ;; Is it better to return NIL if argument is not of type unicode?
  (case unicode
    ((utf-8 utf-16 utf-32)
     unicode)
    (string
     +string-unicode-type+)
    (otherwise
     (check-type unicode unicode)
     (typecase unicode
       (utf-8 'utf-8)
       (utf-16 'utf-16)
       (utf-32 'utf-32)))))


(defun same-unicode-type-p (unicode type)
  (when type
    (or (and (stringp unicode)
             (or (eq 'string type)
                 (stringp type)))
        (typep unicode (unicode-type type)))))


;; Code unit interface

(defgeneric unicode-length (unicode))


(defgeneric u8ref (unicode index))
(defgeneric u16ref (unicode index))
(defgeneric u32ref (unicode index))


(defgeneric (setf u8ref) (code-unit unicode index))
(defgeneric (setf u16ref) (code-unit unicode index))
(defgeneric (setf u32ref) (code-unit unicode index))


(defun unicode-ref (unicode index)
  (etypecase unicode
    (utf-8 (u8ref unicode index))
    (utf-16 (u16ref unicode index))
    (utf-32 (u32ref unicode index))))


(defun (setf unicode-ref) (code-unit unicode index)
  (etypecase unicode
    (utf-8 (setf (u8ref unicode index) code-unit))
    (utf-16 (setf (u16ref unicode index) code-unit))
    (utf-32 (setf (u32ref unicode index) code-unit))))


(defgeneric utf-8-replace (unicode1 unicode2 &key start1 end1 start2 end2)
  (:method (unicode1 unicode2 &key start1 end1 start2 end2)
    (check-type unicode1 utf-8)
    (check-type unicode2 utf-8)
    (loop for index1 from (or start1 0) below (or end1 (unicode-length unicode1))
          for index2 from (or start2 0) below (or end2 (unicode-length unicode2))
          do (setf (u8ref unicode1 index1) (u8ref unicode2 index2)))))


(defgeneric utf-16-replace (unicode1 unicode2 &key start1 end1 start2 end2)
  (:method (unicode1 unicode2 &key start1 end1 start2 end2)
    (check-type unicode1 utf-16)
    (check-type unicode2 utf-16)
    (loop for index1 from (or start1 0) below (or end1 (unicode-length unicode1))
          for index2 from (or start2 0) below (or end2 (unicode-length unicode2))
          do (setf (u16ref unicode1 index1) (u16ref unicode2 index2)))))


(defgeneric utf-32-replace (unicode1 unicode2 &key start1 end1 start2 end2)
  (:method (unicode1 unicode2 &key start1 end1 start2 end2)
    (check-type unicode1 utf-32)
    (check-type unicode2 utf-32)
    (loop for index1 from (or start1 0) below (or end1 (unicode-length unicode1))
          for index2 from (or start2 0) below (or end2 (unicode-length unicode2))
          do (setf (u32ref unicode1 index1) (u32ref unicode2 index2)))))


(defun unicode-replace (unicode1 unicode2 &key start1 end1 start2 end2)
  (check-type unicode1 unicode)
  (check-type unicode2 unicode)
  (etypecase unicode1
    (utf-8
     (utf-8-replace unicode1 unicode2 :start1 start1 :end1 end1 :start2 start2 :end2 end2))
    (utf-16
     (utf-16-replace unicode1 unicode2 :start1 start1 :end1 end1 :start2 start2 :end2 end2))
    (utf-32
     (utf-32-replace unicode1 unicode2 :start1 start1 :end1 end1 :start2 start2 :end2 end2))))


(defgeneric clone-unicode (unicode)
  (:method (unicode)
    (check-type unicode unicode)
    (let ((new (funcall (unicode-constructor unicode) (unicode-length unicode))))
      (unicode-replace new unicode)
      new)))


;; Code point interface

(deftype errors-designator ()
  `(member nil :default :strict :replace :ignore))


(defvar *transform-errors-default* :strict)


(defun defaulted-transform-errors (errors)
  (check-type errors errors-designator)
  (if (member errors '(nil :default))
      *transform-errors-default*
      errors))


(defun strictp (errors)
  (eq :strict (defaulted-transform-errors errors)))


(defun replace-when-strict (errors)
  (ecase (defaulted-transform-errors errors)
    (:ignore
     :ignore)
    ((:replace :strict)
     :replace)))


(define-condition unicode-transform-error (error)
  ((unicode :initarg :unicode :reader unicode-transform-error-unicode)
   (control :initarg :control :reader unicode-transform-error-control)
   (arguments :initarg :arguments :reader unicode-transform-error-arguments))
  (:report (lambda (c stream)
             (apply #'format stream
                    (unicode-transform-error-control c)
                    (unicode-transform-error-arguments c)))))


(defmacro with-transform-error ((unicode index errors) &body body)
  `(block nil
     (flet ((transform-error (control &rest arguments)
              (ecase (defaulted-transform-errors ,errors)
                ((:replace)
                 (return (values #xFFFD ,index t)))
                (:ignore
                 (return (values #xFFFD ,index t)))
                (:strict
                 (error 'unicode-transform-error :unicode ,unicode :control control :arguments arguments)))))
       (restart-case
           ,@body
         (replace ()
           :report "Return replacement character U+FFFD."
           (values #xFFFD ,index t))))))


(defun code-point-at-utf-8 (unicode index &key errors)
  (check-type unicode utf-8)
  (check-type errors errors-designator)
  (with-transform-error (unicode index errors)
    (let ((code-point (u8ref unicode index))
          (lower #x80)
          (upper #XBF))
      (flet ((cbyte ()
               (unless (< index (unicode-length unicode))
                 (transform-error "End of string in UTF-8 sequence."))
               (let ((byte (u8ref unicode index)))
                 (unless (<= lower byte upper)
                   (transform-error "Invalid UTF-8 scalar ~X" byte))
                 (setf code-point (logxor (ash code-point 6)
                                          (ldb (byte 6 0) byte)))
                 (setf lower #x80)
                 (setf upper #xBF)
                 (incf index))))
        (incf index)
        (typecase code-point
          ((integer #x00 #x7F))
          ((integer #xC2 #xDF)
           (setf code-point (ldb (byte 6 0) code-point))
           (cbyte))
          ((integer #xE0 #xEF)
           (setf code-point (ldb (byte 5 0) code-point))
           (when (= #xE0 code-point)
             (setf lower #xA0))
           (when (= #xED code-point)
             (setf upper #x9F))
           (cbyte)
           (cbyte))
          ((integer #xF0 #xF4)
           (setf code-point (ldb (byte 4 0) code-point))
           (when (= #xF0 code-point)
             (setf lower #x90))
           (when (= #xED code-point)
             (setf upper #x8F))
           (cbyte)
           (cbyte)
           (cbyte))
          (t
           (transform-error "Invalid UTF-8 scalar ~X" code-point))))
      (values code-point index))))


(defun code-point-before-utf-8 (unicode index &key errors)
  (check-type unicode utf-8)
  (check-type errors errors-designator)
  (if (= 1 index)
      (code-point-at-utf-8 unicode 0 :errors errors)
      (loop for prev-index from (max 0 (- index 4)) below index
            do (multiple-value-bind (code-point next-index invalid)
                   (code-point-at-utf-8 unicode prev-index :errors :replace)
                 (when (<= index next-index)
                   (when (and invalid (strictp errors))
                     (multiple-value-setq (code-point next-index invalid)
                       (code-point-at-utf-8 unicode prev-index :errors errors)))
                   (return (values code-point prev-index invalid)))))))


(defun code-point-at-utf-16 (unicode index &key errors)
  (check-type unicode utf-16)
  (check-type errors errors-designator)
  (with-transform-error (unicode index errors)
    (let ((lead (u16ref unicode index)))
      (incf index)
      (cond ((<= #xD800 lead #xDBFF)
             (unless (< index (unicode-length unicode))
               (transform-error "End of string in UTF-16 sequence."))
             (let ((tail (u16ref unicode index)))
               (unless (<= #xDC00 tail #xDFFF)
                 (transform-error "Invalid UTF-16 tail ~X" tail))
               (incf index)
               (values (+ #x10000
                          (ash (- lead #xD800) 10)
                          (- tail #xDC00))
                       index)))
            (t
             (when (<= #xDC00 lead #xDFFF)
               (transform-error "Lone UTF-16 tail surrogate ~X" lead))
             (values lead index))))))


(defun code-point-before-utf-16 (unicode index &key errors)
  (check-type unicode utf-16)
  (check-type errors errors-designator)
  (let ((prev-index
          (cond ((= index 1)
                 0)
                ((and (<= 2 index)
                      (<= #xDC00 (u16ref unicode (- index 1)) #xDFFF)
                      (<= #xD800 (u16ref unicode (- index 2)) #xDBFF))
                 (- index 2))
                (t
                 (1- index)))))
    (multiple-value-bind (code-point next-index invalid)
        (code-point-at-utf-16 unicode prev-index :errors errors)
      (declare (ignore next-index))
      (values code-point prev-index invalid))))


(defun code-point-at-utf-32 (unicode index &key errors)
  (check-type unicode utf-32)
  (check-type errors errors-designator)
  (with-transform-error (unicode index errors)
    (let ((code-point (u32ref unicode index)))
      (incf index)
      (unless (typep code-point 'unicode-scalar)
        (transform-error "Surrogate code point in UTF-32 ~X" code-point))
      (values code-point index))))


(defun code-point-before-utf-32 (unicode index &key errors)
  (check-type unicode utf-32)
  (check-type errors errors-designator)
  (code-point-at-utf-32 unicode (1- index) :errors errors))


(defun code-point-at (unicode index &key errors)
  (check-type unicode unicode)
  (check-type errors errors-designator)
  (etypecase unicode
    (utf-8 (code-point-at-utf-8 unicode index :errors errors))
    (utf-16 (code-point-at-utf-16 unicode index :errors errors))
    (utf-32 (code-point-at-utf-32 unicode index :errors errors))))


(defun code-point-before (unicode index &key errors)
  (check-type unicode unicode)
  (check-type errors errors-designator)
  (etypecase unicode
    (utf-8 (code-point-before-utf-8 unicode index :errors errors))
    (utf-16 (code-point-before-utf-16 unicode index :errors errors))
    (utf-32 (code-point-before-utf-32 unicode index :errors errors))))


(defun set-code-point (unicode index code-point)
  (check-type unicode unicode)
  (check-type code-point code-point)
  (etypecase unicode
    (utf-8
     (cond ((< code-point #x80)
            (setf (u8ref unicode index) code-point)
            (1+ index))
           ((< code-point #x800)
            (setf (u8ref unicode index)
                  (logxor #b11000000 (ldb (byte 5 6) code-point)))
            (setf (u8ref unicode (+ 1 index))
                  (logxor #b10000000 (ldb (byte 6 0) code-point)))
            (+ 2 index))
           ((< code-point #x10000)
            (setf (u8ref unicode index)
                  (logxor #b11100000 (ldb (byte 4 12) code-point)))
            (setf (u8ref unicode (+ 1 index))
                  (logxor #b10000000 (ldb (byte 6 6) code-point)))
            (setf (u8ref unicode (+ 2 index))
                  (logxor #b10000000 (ldb (byte 6 0) code-point)))
            (+ 3 index))
           (t
            (setf (u8ref unicode index)
                  (logxor #b11110000 (ldb (byte 3 18) code-point)))
            (setf (u8ref unicode (+ 1 index))
                  (logxor #b10000000 (ldb (byte 6 12) code-point)))
            (setf (u8ref unicode (+ 2 index))
                  (logxor #b10000000 (ldb (byte 6 6) code-point)))
            (setf (u8ref unicode (+ 3 index))
                  (logxor #b10000000 (ldb (byte 6 0) code-point)))
            (+ 4 index))))
    (utf-16
     (cond ((<= code-point #xFFFF)
            (setf (u16ref unicode index) code-point)
            (1+ index))
           (t
            (setf (u16ref unicode index) (+ #xD7C0 (ldb (byte 10 10) code-point)))
            (setf (u16ref unicode (1+ index)) (+ #xDC00 (ldb (byte 10 0) code-point)))
            (+ 2 index))))
    (utf-32
     (setf (u32ref unicode index) code-point)
     (1+ index))))


(defun next-code-point (unicode index &key errors)
  (check-type unicode unicode)
  (check-type errors errors-designator)
  (multiple-value-bind (code-point index invalid)
      (code-point-at unicode index :errors errors)
    (declare (ignore code-point))
    (values index invalid)))


(defun code-point-count (unicode &key errors)
  (check-type unicode unicode)
  (check-type errors errors-designator)
  (loop with errors = (replace-when-strict errors)
        with invalid
        with index = 0
        while (< index (unicode-length unicode))
        do (multiple-value-setq (index invalid) (next-code-point unicode index :errors errors))
        count (not (eq :ignore invalid))))


(defmacro do-code-points ((var unicode &key errors) &body body)
  (let ((%unicode (gensym "UNICODE"))
        (%errors (gensym "ERRORS"))
        (%index (gensym "INDEX"))
        (%should-ignore (gensym "SHOULD-IGNORE"))
        (invalid (gensym "INVALID")))
    `(loop with ,var of-type code-point = 0
           with ,%unicode = ,unicode
           with ,%errors = ,errors
           with ,%should-ignore = (eq :ignore (defaulted-transform-errors ,errors))
           with ,invalid
           with ,%index fixnum = 0
             initially (check-type ,%unicode unicode)
                       (check-type ,%errors errors-designator)
           while (< ,%index (the fixnum (unicode-length ,%unicode)))
           do (multiple-value-setq (,var ,%index ,invalid) (code-point-at ,%unicode ,%index :errors ,%errors))
           unless (and ,%should-ignore ,invalid)
             do (progn ,@body))))


(defun map-code-points (function unicode &key errors)
  (declare (optimize (speed 3)))
  (check-type function function)
  (check-type unicode unicode)
  (check-type errors errors-designator)
  (do-code-points (code-point unicode :errors errors)
    (funcall function code-point)))


(defun well-formed-p (unicode)
  (check-type unicode unicode)
  (handler-case
      (progn
        (do-code-points (v unicode :errors :strict))
        t)
    (unicode-transform-error ()
      nil)))


(defun code-point-utf-8-length (code-point)
  (check-type code-point code-point)
  (cond ((< code-point #x80)
         1)
        ((< code-point #x800)
         2)
        ((< code-point #x10000)
         3)
        (t
         4)))


(defun code-point-utf-16-length (code-point)
  (check-type code-point code-point)
  (if (<= code-point #xFFFF)
      1
      2))


(deftype unicode-type-designator ()
  `(or unicode
       (member string utf-8 utf-16 utf-32)))


(defun code-point-length (target code-point)
  (check-type target unicode-type-designator)
  (check-type code-point code-point)
  (case target
    (utf-8 (code-point-utf-8-length code-point))
    (utf-16 (code-point-utf-16-length code-point))
    (utf-32 1)
    (otherwise
     (code-point-length (nth-value 1 (unicode-constructor target)) code-point))))


(defun utf-8-length (unicode &key errors)
  (check-type unicode unicode)
  (check-type errors errors-designator)
  (let ((length 0))
    (do-code-points (code-point unicode :errors (replace-when-strict errors))
      (incf length (code-point-utf-8-length code-point)))
    length))


(defun utf-16-length (unicode &key errors)
  (check-type unicode unicode)
  (check-type errors errors-designator)
  (let ((length 0))
    (do-code-points (code-point unicode :errors (replace-when-strict errors))
      (incf length (code-point-utf-16-length code-point)))
    length))


(defun utf-32-length (unicode &key errors)
  (check-type unicode unicode)
  (check-type errors errors-designator)
  (code-point-count unicode :errors (replace-when-strict errors)))


(defun unicode-length-for (target source &key errors)
  (check-type target unicode-type-designator)
  (check-type source unicode)
  (check-type errors errors-designator)
  (case target
    (utf-8 (utf-8-length source :errors errors))
    (utf-16 (utf-16-length source :errors errors))
    (utf-32 (utf-32-length source :errors errors))
    (otherwise
     (unicode-length-for (nth-value 1 (unicode-constructor target)) source :errors errors))))


;; Strings as unicode text

(defmethod clone-unicode ((string string))
  (copy-seq string))


(defun make-unicode-string (count &key (initial-contents nil initial-contents-p) initial-element)
  (if initial-contents-p
      (map 'string #'code-char initial-contents)
      (make-string count :initial-element (code-char (or initial-element 0)))))


;; String as unicode

#+string-is-utf-8
(defmethod utf-8-p ((unicode string))
  t)

#+string-is-utf-16
(defmethod utf-16-p ((unicode string))
  t)


#+string-is-utf-32
(defmethod utf-32-p ((unicode string))
  t)


(defmethod unicode-length ((unicode string))
  (length unicode))


#+string-is-utf-8
(defmethod u8ref ((unicode string) index)
  (char-code (char unicode index)))


#+string-is-utf-16
(defmethod u16ref ((unicode string) index)
  (char-code (char unicode index)))


#+string-is-utf-32
(defmethod u32ref ((unicode string) index)
  (char-code (char unicode index)))


#+string-is-utf-8
(defmethod (setf u8ref) (code-unit (unicode string) index)
  (setf (char unicode index) (code-char code-unit))
  code-unit)

#+string-is-utf-16
(defmethod (setf u16ref) (code-unit (unicode string) index)
  (setf (char unicode index) (code-char code-unit))
  code-unit)

#+string-is-utf-32
(defmethod (setf u32ref) (code-unit (unicode string) index)
  (setf (char unicode index) (code-char code-unit))
  code-unit)


;; Character as unicode

#+string-is-utf-8
(defmethod utf-8-p ((character character))
  t)


#+string-is-utf-16
(defmethod utf-16-p ((character character))
  t)


#+string-is-utf-32
(defmethod utf-32-p ((character character))
  t)


(defmethod unicode-length ((character character))
  1)


#+string-is-utf-8
(defmethod u8ref ((character character) index)
  (assert (zerop index) (index)
          "u8ref: Invalid index ~A for character ~S as unicode. Must be 0."
          index
          character)
  (char-code character))

#+string-is-utf-16
(defmethod u16ref ((character character) index)
  (assert (zerop index) (index)
          "u16ref: Invalid index ~A for character ~S as unicode. Must be 0."
          index
          character)
  (char-code character))


#+string-is-utf-32
(defmethod u32ref ((character character) index)
  (assert (zerop index) (index)
          "u32ref: Invalid index ~A for character ~S as unicode. Must be 0."
          index
          character)
  (char-code character))


;; utf-8 unicode

(defstruct %utf-8
  (data nil :type (vector utf-8-code-unit)))


(defmethod make-load-form ((unicode %utf-8) &optional environment)
  (declare (ignore environment))
  `(make-%utf-8 :data ,(%utf-8-data unicode)))


(defun make-utf-8 (count &key (initial-contents nil initial-contents-p) initial-element)
  (make-%utf-8 :data (if initial-contents-p
                          (make-array count :element-type 'utf-8-code-unit
                                            :initial-contents initial-contents)
                          (make-array count :element-type 'utf-8-code-unit
                                            :initial-element (or initial-element 0)))))


(defmethod utf-8-p ((unicode %utf-8)) t)


(defmethod unicode-length ((unicode %utf-8))
  (length (%utf-8-data unicode)))


(defmethod u8ref ((unicode %utf-8) index)
  (aref (%utf-8-data unicode) index))


(defmethod (setf u8ref) (code-unit (unicode %utf-8) index)
  (check-type code-unit utf-8-code-unit)
  (setf (aref (%utf-8-data unicode) index) code-unit))


;; utf-16 unicode

(defstruct %utf-16
  (data nil :type (vector utf-16-code-unit)))


(defmethod make-load-form ((unicode %utf-16) &optional environment)
  (declare (ignore environment))
  `(make-%utf-16 :data ,(%utf-16-data unicode)))


(defun make-utf-16 (count &key (initial-contents nil initial-contents-p) initial-element)
  (make-%utf-16 :data (if initial-contents-p
                          (make-array count :element-type 'utf-16-code-unit
                                            :initial-contents initial-contents)
                          (make-array count :element-type 'utf-16-code-unit
                                            :initial-element (or initial-element 0)))))


(defmethod utf-16-p ((unicode %utf-16)) t)


(defmethod unicode-length ((unicode %utf-16))
  (length (%utf-16-data unicode)))


(defmethod u16ref ((unicode %utf-16) index)
  (aref (%utf-16-data unicode) index))


(defmethod (setf u16ref) (code-unit (unicode %utf-16) index)
  (setf (aref (%utf-16-data unicode) index) code-unit))


;; utf-32 unicode

(defstruct %utf-32
  (data nil :type (vector utf-32-code-unit)))


(defmethod make-load-form ((unicode %utf-32) &optional environment)
  (declare (ignore environment))
  `(make-%utf-32 :data ,(%utf-32-data unicode)))


(defun make-utf-32 (count &key (initial-contents nil initial-contents-p) initial-element)
  (make-%utf-32 :data (if initial-contents-p
                          (make-array count :element-type 'utf-32-code-unit
                                            :initial-contents initial-contents)
                          (make-array count :element-type 'utf-32-code-unit
                                            :initial-element (or initial-element 0)))))


(defmethod utf-32-p ((unicode %utf-32)) t)


(defmethod unicode-length ((unicode %utf-32))
  (length (%utf-32-data unicode)))


(defmethod u32ref ((unicode %utf-32) index)
  (aref (%utf-32-data unicode) index))


(defmethod (setf u32ref) (code-unit (unicode %utf-32) index)
  (setf (aref (%utf-32-data unicode) index) code-unit))


;; unicode constructors

(defun unicode-constructor (type)
  (check-type type unicode-type-designator)
  (etypecase type
    (string (values #'make-unicode-string +string-unicode-type+))
    (utf-8 (values #'make-utf-8 'utf-8))
    (utf-16 (values #'make-utf-16 'utf-16))
    (utf-32 (values #'make-utf-32 'utf-32))
    (symbol
     (ecase type
       (string (values #'make-unicode-string +string-unicode-type+))
       (utf-8 (values #'make-utf-8 'utf-8))
       (utf-16 (values #'make-utf-16 'utf-16))
       (utf-32 (values #'make-utf-32 'utf-32))))))


(defvar *default-unicode-type* 'string)


(defun make-unicode (count &key type
                             (initial-contents nil initial-contents-p)
                             (initial-element nil initial-element-p))
  (cond ((and initial-element-p initial-contents-p)
         (error "Can't specify both :INITIAL-ELEMENT and :INITIAL-CONTENTS"))
        (initial-element-p
         (funcall (unicode-constructor (or type *default-unicode-type*))
                  count
                  :initial-element initial-element))
        (initial-contents-p
         (funcall (unicode-constructor (or type *default-unicode-type*))
                  (or count (length initial-contents))
                  :initial-contents initial-contents))
        (t
         (funcall (unicode-constructor (or type *default-unicode-type*)) count))))


(defun code-point (code-point &key type)
  (check-type code-point code-point)
  (check-type type (or null unicode-type-designator))
  (let* ((type (or type *default-unicode-type*))
         (unicode (make-unicode (code-point-length type code-point)
                                :type type)))
    (set-code-point unicode 0 code-point)
    unicode))


(defun unicode (unicode &key type errors)
  (check-type unicode unicode)
  (check-type type (or null unicode-type-designator))
  (check-type errors errors-designator)
  (if (same-unicode-type-p unicode type)
      unicode
      (copy-unicode unicode :type type :errors errors)))


(defun copy-unicode (unicode &key type errors)
  (check-type unicode unicode)
  (check-type type (or null unicode-type-designator))
  (check-type errors errors-designator)
  (if (same-unicode-type-p unicode type)
      (clone-unicode unicode)
      (let* ((new-type (or type *default-unicode-type*))
             (new (make-unicode (unicode-length-for new-type unicode :errors errors)
                               :type new-type))
            (index 0))
        (do-code-points (code-point unicode :errors errors)
          (setf index (set-code-point new index code-point)))
        new)))


(defun utf-8 (unicode &key errors)
  (unicode unicode :errors errors :type 'utf-8))


(defun utf-16 (unicode &key errors)
  (unicode unicode :errors errors :type 'utf-16))


(defun utf-32 (unicode &key errors)
  (unicode unicode :errors errors :type 'utf-32))


(defun unicode-string (unicode &key errors)
  (unicode unicode :errors errors :type 'string))


(defun copy-utf-8 (unicode &key errors)
  (copy-unicode unicode :type 'utf-8 :errors errors))


(defun copy-utf-16 (unicode &key errors)
  (copy-unicode unicode :type 'utf-16 :errors errors))


(defun copy-utf-32 (unicode &key errors)
  (copy-unicode unicode :type 'utf-32 :errors errors))


(defun copy-unicode-string (unicode &key errors)
  (copy-unicode unicode :type 'string :errors errors))


(defun concatenate-unicode-of-same-type (unicode-list)
  (let ((new (funcall (unicode-constructor (car unicode-list))
                      (reduce #'+ unicode-list :key #'unicode-length))))
    (loop with index = 0
          for src in unicode-list
          do (unicode-replace new src :start1 index)
             (incf index (unicode-length src)))
    new))


(defun concatenate-unicode (list &key type errors copy)
  (check-type list list)
  (check-type type (or null unicode-type-designator))
  (check-type errors errors-designator)
  (cond ((null list)
         (make-unicode 0 :type type))
        ((null (cdr list))
         (if (and (not copy)
                  (or (null type)
                      (typep (car list) (unicode-type type))))
             (car list)
             (copy-unicode (car list) :type type :errors errors)))
        (t
         (let* ((same-type nil)
                (src-list (loop while list
                                for group-type = (unicode-type (car list))
                                for group = (loop for elt = (car list)
                                                  while (typep elt group-type)
                                                  collect (pop list))
                                for first = t then nil
                                do (if first
                                       (setf same-type group-type)
                                       (unless (eq group-type same-type)
                                         (setf same-type nil)))
                                collect (concatenate-unicode-of-same-type group))))
           (multiple-value-bind (constructor type-name)
               (unicode-constructor (or type
                                        (when same-type (car src-list))
                                        *default-unicode-type*))
             (let* ((length (loop for src in src-list
                                  summing (if (typep src type-name)
                                              (unicode-length src)
                                              (unicode-length-for type-name src :errors errors))))
                    (new (funcall constructor length)))
               (loop with index = 0
                     for src in src-list
                     do (cond ((typep src type-name)
                               (unicode-replace new src :start1 index)
                               (incf index (unicode-length src)))
                              (t
                               (do-code-points (code-point src :errors errors)
                                 (setf index (set-code-point new index code-point))))))
               new))))))


;; printing unicode unreadable

(defun print-unicode-code-units (stream unicode type code-units digits)
  (print-unreadable-object (unicode stream)
    (princ type stream)
    (loop for byte across code-units
          do (format stream " ~v,'0X" digits byte))))


(defmethod print-object ((unicode %utf-8) stream)
  (print-unicode-code-units stream unicode 'utf-8 (%utf-8-data unicode) 2))


(defmethod print-object ((unicode %utf-16) stream)
  (print-unicode-code-units stream unicode 'utf-16 (%utf-16-data unicode) 4))


(defmethod print-object ((unicode %utf-32) stream)
  (print-unicode-code-units stream unicode 'utf-32 (%utf-32-data unicode) 4))
