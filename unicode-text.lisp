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

(defun unicode-type (unicode)
  (etypecase unicode
    (utf-8 'utf-8)
    (utf-16 'utf-16)
    (utf-32 'utf-32)))

(defun unicodep (object)
  (or (utf-8-p object)
      (utf-16-p object)
      (utf-32-p object)))
(deftype unicode () '(satisfies unicodep))


;; Code unit interface

(defgeneric unicode-length (unicode)
  (:method (thing)
    (error "~S: ~S in not of type unicode." 'unicode-length thing)))

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
    (loop for index1 from (or start1 0) below (or end1 (unicode-length unicode1))
          for index2 from (or start2 0) below (or end2 (unicode-length unicode2))
          do (setf (u8ref unicode1 index1) (u8ref unicode2 index2)))))

(defgeneric utf-16-replace (unicode1 unicode2 &key start1 end1 start2 end2)
  (:method (unicode1 unicode2 &key start1 end1 start2 end2)
    (loop for index1 from (or start1 0) below (or end1 (unicode-length unicode1))
          for index2 from (or start2 0) below (or end2 (unicode-length unicode2))
          do (setf (u16ref unicode1 index1) (u16ref unicode2 index2)))))

(defgeneric utf-32-replace (unicode1 unicode2 &key start1 end1 start2 end2)
  (:method (unicode1 unicode2 &key start1 end1 start2 end2)
    (loop for index1 from (or start1 0) below (or end1 (unicode-length unicode1))
          for index2 from (or start2 0) below (or end2 (unicode-length unicode2))
          do (setf (u32ref unicode1 index1) (u32ref unicode2 index2)))))

(defun unicode-replace (unicode1 unicode2 &key start1 end1 start2 end2)
  (etypecase unicode1
    (utf-8
     (check-type unicode2 utf-8)
     (utf-8-replace unicode1 unicode2 :start1 start1 :end1 end1 :start2 start2 :end2 end2))
    (utf-16
     (check-type unicode2 utf-16)
     (utf-16-replace unicode1 unicode2 :start1 start1 :end1 end1 :start2 start2 :end2 end2))
    (utf-32
     (check-type unicode2 utf-32)
     (utf-32-replace unicode1 unicode2 :start1 start1 :end1 end1 :start2 start2 :end2 end2))))

;; Code point interface

(defvar *transform-errors-default* :strict)

(defun defaulted-transform-errors (errors)
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

(defmacro with-transform-error (&body body)
  `(block nil
     (flet ((transform-error (control &rest arguments)
              (ecase (defaulted-transform-errors errors)
                ((:replace)
                 (return (values #xFFFD index t)))
                (:ignore
                 (return (values #xFFFD index t)))
                (:strict
                 (error 'unicode-transform-error :unicode unicode :control control :arguments arguments)))))
       (restart-case
           ,@body
         (replace ()
           :report "Return replacement character U+FFFD."
           (values #xFFFD index t))))))


(defun code-point-at-utf-8 (unicode index &key errors)
  (check-type unicode utf-8)
  (with-transform-error
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
  (with-transform-error
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
  (with-transform-error
    (let ((code-point (u32ref unicode index)))
      (incf index)
      (unless (typep code-point 'unicode-scalar)
        (transform-error "Surrogate code point in UTF-32 ~X" code-point))
      (values code-point index))))


(defun code-point-before-utf-32 (unicode index &key errors)
  (code-point-at-utf-32 unicode (1- index) :errors errors))


(defun code-point-at (unicode index &key errors)
  (etypecase unicode
    (utf-8 (code-point-at-utf-8 unicode index :errors errors))
    (utf-16 (code-point-at-utf-16 unicode index :errors errors))
    (utf-32 (code-point-at-utf-32 unicode index :errors errors))))


(defun code-point-before (unicode index &key errors)
  (etypecase unicode
    (utf-8 (code-point-before-utf-8 unicode index :errors errors))
    (utf-16 (code-point-before-utf-16 unicode index :errors errors))
    (utf-32 (code-point-before-utf-32 unicode index :errors errors))))


(defun set-code-point (unicode index code-point)
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
  (multiple-value-bind (code-point index invalid)
      (code-point-at unicode index :errors errors)
    (declare (ignore code-point))
    (values index invalid)))

(defun code-point-count (unicode &key errors)
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
           while (< ,%index (the fixnum (unicode-length ,%unicode)))
           do (multiple-value-setq (,var ,%index ,invalid) (code-point-at ,%unicode ,%index :errors ,%errors))
           unless (and ,%should-ignore ,invalid)
             do (progn ,@body))))

(defun map-code-points (function unicode &key errors)
  (declare (optimize (speed 3))
           (function function))
  (do-code-points (code-point unicode :errors errors)
    (funcall function code-point)))

(defun well-formed-p (unicode)
  (handler-case
      (progn
        (do-code-points (v unicode :errors :strict))
        t)
    (unicode-transform-error ()
      nil)))

(defun code-point-utf-8-length (code-point)
  (cond ((< code-point #x80)
         1)
        ((< code-point #x800)
         2)
        ((< code-point #x10000)
         3)
        (t
         4)))

(defun code-point-utf-16-length (code-point)
  (if (<= code-point #xFFFF)
      1
      2))

(defun code-point-length (target code-point)
  (case target
    (utf-8 (code-point-utf-8-length code-point))
    (utf-16 (code-point-utf-16-length code-point))
    (utf-32 1)
    (otherwise
     (code-point-length (nth-value 1 (unicode-constructor target)) code-point))))

(defun utf-8-length (unicode &key errors)
  (let ((length 0))
    (do-code-points (code-point unicode :errors (replace-when-strict errors))
      (incf length (code-point-utf-8-length code-point)))
    length))

(defun utf-16-length (unicode &key errors)
  (let ((length 0))
    (do-code-points (code-point unicode :errors (replace-when-strict errors))
      (incf length (code-point-utf-16-length code-point)))
    length))

(defun utf-32-length (unicode &key errors)
  (code-point-count unicode :errors (replace-when-strict errors)))



(defun unicode-length-for (target source &key errors)
  (case target
    (utf-8 (utf-8-length source :errors errors))
    (utf-16 (utf-16-length source :errors errors))
    (utf-32 (utf-32-length source :errors errors))
    (otherwise
     (unicode-length-for (nth-value 1 (unicode-constructor target)) source :errors errors))))


;; Strings as unicode text

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+force-utf-8-strings (pushnew :utf-8-strings *features*)
  #+force-utf-16-strings (pushnew :utf-16-strings *features*)
  #+force-utf-32-strings (pushnew :utf-32-strings *features*)

  #-(or force-utf-8-strings
        force-utf-16-strings
        force-utf-32-strings)
  (cond ((<= 1114112 char-code-limit)
         (pushnew :utf-32-strings *features*))
        ((= #x10000 char-code-limit)
         (pushnew :utf-16-strings *features*))
        ((<= 256 char-code-limit)
         (pushnew :utf-8-strings *features*))))


;; String as utf-8
#+utf-8-strings
(progn
  (defconstant +string-unicode-type+ 'utf-8)

  (defmethod utf-8-p ((unicode string)) t)

  (defmethod unicode-length ((unicode string))
    (length unicode))

  (defmethod u8ref ((unicode string) index)
    (char-code (char unicode index)))

  (defmethod (setf u8ref) (code-unit (unicode string) index)
    (setf (char unicode index) (code-char code-unit))
    code-unit)

  ;; Character as utf-8

  (defmethod utf-8-p ((character character)) t)

  (defmethod unicode-length ((character character))
    1)

  (defmethod u8ref ((character character) index)
    (assert (zerop index) (index)
            "u8ref: Invalid index ~A for character ~S as unicode. Must be 0."
            index
            character)
    (char-code character)))


;; String as utf-16
#+utf-16-strings
(progn
  (defconstant +string-unicode-type+ 'utf-16)

  (defmethod utf-16-p ((unicode string)) t)

  (defmethod unicode-length ((unicode string))
    (length unicode))

  (defmethod u16ref ((unicode string) index)
    (char-code (char unicode index)))

  (defmethod (setf u16ref) (code-unit (unicode string) index)
    (setf (char unicode index) (code-char code-unit))
    code-unit)

  ;; Character as utf-16

  (defmethod utf-16-p ((character character)) t)

  (defmethod unicode-length ((character character))
    1)

  (defmethod u16ref ((character character) index)
    (assert (zerop index) (index)
            "u16ref: Invalid index ~A for character ~S as unicode. Must be 0."
            index
            character)
    (char-code character)))


;; String as utf-32
#+utf-32-strings
(progn
  (defconstant +string-unicode-type+ 'utf-32)

  (defmethod utf-32-p ((unicode string)) t)

  (defmethod unicode-length ((unicode string))
    (length unicode))

  (defmethod u32ref ((unicode string) index)
    (char-code (char unicode index)))

  (defmethod (setf u32ref) (code-unit (unicode string) index)
    (setf (char unicode index) (code-char code-unit))
    code-unit)

  ;; Character as utf-32

  (defmethod utf-32-p ((character character)) t)

  (defmethod unicode-length ((character character))
    1)

  (defmethod u32ref ((character character) index)
    (assert (zerop index) (index)
            "u32ref: Invalid index ~A for character ~S as unicode. Must be 0."
            index
            character)
    (char-code character)))

;; utf-8 unicode

(defstruct %utf-8
  (data nil :type (vector utf-8-code-unit)))

(defmethod make-load-form ((unicode %utf-8) &optional environment)
  (declare (ignore environment))
  `(make-%utf-8 :data ,(%utf-8-data unicode)))

(defun make-utf-8 (count &key (initial-contents nil initial-contents-p))
  (make-%utf-8 :data (if initial-contents-p
                          (make-array count :element-type 'utf-8-code-unit
                                            :initial-contents initial-contents)
                          (make-array count :element-type 'utf-8-code-unit))))

(defmethod utf-8-p ((unicode %utf-8)) t)

(defmethod unicode-length ((unicode %utf-8))
  (length (%utf-8-data unicode)))

(defmethod u8ref ((unicode %utf-8) index)
  (aref (%utf-8-data unicode) index))

(defmethod (setf u8ref) (code-unit (unicode %utf-8) index)
  (setf (aref (%utf-8-data unicode) index) code-unit))

;; utf-16 unicode

(defstruct %utf-16
  (data nil :type (vector utf-16-code-unit)))

(defmethod make-load-form ((unicode %utf-16) &optional environment)
  (declare (ignore environment))
  `(make-%utf-16 :data ,(%utf-16-data unicode)))

(defun make-utf-16 (count &key (initial-contents nil initial-contents-p))
  (make-%utf-16 :data (if initial-contents-p
                          (make-array count :element-type 'utf-16-code-unit
                                            :initial-contents initial-contents)
                          (make-array count :element-type 'utf-16-code-unit))))

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

(defun make-utf-32 (count &key (initial-contents nil initial-contents-p))
  (make-%utf-32 :data (if initial-contents-p
                          (make-array count :element-type 'utf-32-code-unit
                                            :initial-contents initial-contents)
                          (make-array count :element-type 'utf-32-code-unit))))

(defmethod utf-32-p ((unicode %utf-32)) t)

(defmethod unicode-length ((unicode %utf-32))
  (length (%utf-32-data unicode)))

(defmethod u32ref ((unicode %utf-32) index)
  (aref (%utf-32-data unicode) index))

(defmethod (setf u32ref) (code-unit (unicode %utf-32) index)
  (setf (aref (%utf-32-data unicode) index) code-unit))

;; unicode constructors

(defun unicode-constructor (type)
  (etypecase type
    (string (values #'make-string +string-unicode-type+))
    (utf-8 (values #'make-utf-8 'utf-8))
    (utf-16 (values #'make-utf-16 'utf-16))
    (utf-32 (values #'make-utf-32 'utf-32))
    (symbol
     (ecase type
       (string (values #'make-string +string-unicode-type+))
       (utf-8 (values #'make-utf-8 'utf-8))
       (utf-16 (values #'make-utf-16 'utf-16))
       (utf-32 (values #'make-utf-32 'utf-32))))))

(defvar *default-unicode-type* 'string)

(defun make-unicode (count &key type initial-contents)
  (funcall (unicode-constructor (or type *default-unicode-type*))
           count :initial-contents initial-contents))

(defun utf-8 (&rest unicode)
  (copy-unicode unicode :type 'utf-8))

(defun utf-16 (&rest unicode)
  (copy-unicode unicode :type 'utf-16))

(defun utf-32 (&rest unicode)
  (copy-unicode unicode :type 'utf-32))

(defun unicode-string (&rest unicode)
  (copy-unicode unicode :type 'string))

(defun unicode (&rest unicode)
  (copy-unicode unicode))

(defun copy-unicode (data &key type errors)
  (multiple-value-bind (constructor)
      (unicode-constructor (or type *default-unicode-type*))
    (etypecase data
      (unicode
       (concatenate-unicode (list (funcall constructor 0)
                                  data)
                            :type type :errors errors))
      (list
       (unless errors
         (when (member (car data) '(:strict :replace :ignore))
           (setf errors (pop data))))
       (concatenate-unicode
        (if type
            (loop for src in data
                  collect (etypecase src
                            (unicode src)
                            (integer
                             (let ((new (funcall constructor 1)))
                               (setf (unicode-ref new 0) src)
                               new))))
            data)
        :type type :errors errors)))))

(defun concatenate-unicode-of-same-type (unicode-list)
  (let ((new (funcall (unicode-constructor (car unicode-list))
                      (reduce #'+ unicode-list :key #'unicode-length))))
    (loop with index = 0
          for src in unicode-list
          do (unicode-replace new src :start1 index)
             (incf index (unicode-length src)))
    new))

(defun concatenate-unicode (unicode &key type errors)
  (let* ((same-type nil)
         (src-list (loop while unicode
                         for group-type = (unicode-type (car unicode))
                         for group = (loop for elt = (car unicode)
                                           while (typep elt group-type)
                                           collect (pop unicode))
                         for first = t then nil
                         do (if first
                                (setf same-type group-type)
                                (unless (eq group-type same-type)
                                  (setf same-type nil)))
                         collect (concatenate-unicode-of-same-type group))))
    (multiple-value-bind (constructor type-name)
        (unicode-constructor (or type
                                 same-type
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
        new))))


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
