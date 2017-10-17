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


;; Code point interface

(defvar *unicode-transform-error-handling*)

(define-condition unicode-transform-error (error)
  ((unicode :initarg :unicode :reader unicode-transform-error-unicode)
   (control :initarg :control :reader unicode-transform-error-control)
   (arguments :initarg :arguments :reader unicode-transform-error-arguments))
  (:report (lambda (c stream)
             (apply #'format stream
                    (unicode-transform-error-control c)
                    (unicode-transform-error-arguments c)))))

(defun transform-error (unicode control &rest arguments)
  (when (boundp '*unicode-transform-error-handling*)
    ;;(let ((*print-circle* t)) (format t "~&h ~S ~%" (car *unicode-transform-error-handling*)))
    (let ((method (cdr (assoc unicode (car *unicode-transform-error-handling*)))))
      (when method
        (let ((restart (and *unicode-transform-error-handling* (find-restart method))))
          (when restart
            (invoke-restart restart))))))
  (error 'unicode-transform-error :unicode unicode :control control :arguments arguments))

(defmacro with-transform-error-restarts (&body body)
  `(let ((*unicode-transform-error-handling*
           (or (and (boundp '*unicode-transform-error-handling*)
                    *unicode-transform-error-handling*)
               (list (list)))))
     ,@body))

(defun show-restarts-p (c)
  (and (or (null c)
           (typep c 'unicode-transform-error))
       (boundp '*unicode-transform-error-handling*)))

(defgeneric code-point (unicode index)
  (:method (unicode index)
    (with-transform-error-restarts
      (restart-case
          (etypecase unicode
            (utf-8
             (let ((code-point (u8ref unicode index))
                   (lower #x80)
                   (upper #XBF))
               (flet ((cbyte ()
                        (unless (< index (unicode-length unicode))
                          (transform-error unicode "End of string in UTF-8 sequence."))
                        (let ((byte (u8ref unicode index)))
                          (unless (<= lower byte upper)
                            (transform-error unicode "Invalid UTF-8 scalar ~X" byte))
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
                    (transform-error unicode "Invalid UTF-8 scalar ~X" code-point))))
               (values code-point index)))
            (utf-16
             (let ((lead (u16ref unicode index)))
               (incf index)
               (cond ((<= #xD800 lead #xDBFF)
                      (unless (< index (unicode-length unicode))
                        (transform-error unicode "End of string in UTF-16 sequence."))
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
                        (transform-error unicode "Lone UTF-16 tail surrage ~X" lead))
                      (values lead index)))))
            (utf-32
             (let ((code-point (u32ref unicode index)))
               (incf index)
               (unless (typep code-point 'unicode-scalar)
                 (transform-error unicode "Surrogate code point in UTF-32 ~X" code-point))
               (values code-point index))))
        (replace ()
          :report "Use replacement character U+FFFD for invalid code units in this unicode string."
          :test show-restarts-p
          (pushnew (cons unicode 'replace) (car *unicode-transform-error-handling*) :key #'car)
          (values #xFFFD index 'replace))
        (skip ()
          :report "Skip all invalid code units in this unicode string."
          :test show-restarts-p
          (pushnew (cons unicode 'skip) (car *unicode-transform-error-handling*) :key #'car)
          (values #xFFFD index 'skip))))))

(defgeneric set-code-point (unicode index code-point)
  (:method (unicode index code-point)
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
       (cond ((< code-point #xFFFF)
              (setf (u16ref unicode index) code-point)
              (1+ index))
             (t
              (setf (u16ref unicode index) (+ #xD7C0 (ldb (byte 10 10) code-point)))
              (setf (u16ref unicode (1+ index)) (+ #xDC00 (ldb (byte 10 0) code-point)))
              (+ 2 index))))
      (utf-32
       (setf (u32ref unicode index) code-point)
       (1+ index)))))

(defun next-code-point (unicode index)
  (multiple-value-bind (code-point index skip)
      (code-point unicode index)
    (declare (ignore code-point))
    (values index skip)))

(defun code-point-count (unicode)
  (with-transform-error-restarts
    (loop with index = 0
          with skip
          while (< index (unicode-length unicode))
          do (multiple-value-setq (index skip) (next-code-point unicode index))
          count (not (eql 'skip skip)))))

(defmacro do-code-points ((var unicode) &body body)
  (with-transform-error-restarts
    (let ((index (gensym "INDEX"))
          (skip (gensym "SKIP"))
          (unicode-var (gensym "UNICODE-VAR")))
      `(loop with ,skip
             with ,var of-type code-point = 0
             with ,index fixnum = 0
             with ,unicode-var = ,unicode
             while (< ,index (the fixnum (unicode-length ,unicode-var)))
             do (multiple-value-setq (,var ,index ,skip) (code-point ,unicode-var ,index))
             unless (eq ,skip 'skip)
               do (progn ,@body)))))

(defun map-code-points (function unicode)
  (declare (optimize (speed 3))
           (function function))
  (do-code-points (code-point unicode)
    (funcall function code-point)))

(defun utf-8-length (unicode)
  (let ((length 0))
    (do-code-points (code-point unicode)
      (cond ((< code-point #x80)
             (incf length 1))
            ((< code-point #x800)
             (incf length 2))
            ((< code-point #x10000)
             (incf length 3))
            (t
             (incf length 4))))
    length))

(defun utf-16-length (unicode)
  (let ((length 0))
    (do-code-points (code-point unicode)
      (if (<= code-point #xFFFF)
          (incf length 1)
          (incf length 2)))
    length))

(defun utf-32-length (unicode)
  (code-point-count unicode))

(defun unicode-length-for (target source)
  (etypecase target
    ((member string) (unicode-length-for "" source))
    ((or utf-8 (member utf-8)) (utf-8-length source))
    ((or utf-16 (member utf-16)) (utf-16-length source))
    ((or utf-32 (member utf-32)) (utf-32-length source))))


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
  (defmethod utf-8-p ((unicode string)) t)

  (defmethod unicode-length ((unicode string))
    (length unicode))

  (defmethod u8ref ((unicode string) index)
    (char-code (char unicode index)))

  (defmethod (setf u8ref) (code-unit (unicode string) index)
    (setf (char unicode index) (code-char code-unit))
    code-unit))


;; String as utf-16
#+utf-16-strings
(progn
  (defmethod utf-16-p ((unicode string)) t)

  (defmethod unicode-length ((unicode string))
    (length unicode))

  (defmethod u16ref ((unicode string) index)
    (char-code (char unicode index)))

  (defmethod (setf u16ref) (code-unit (unicode string) index)
    (setf (char unicode index) (code-char code-unit))
    code-unit))


;; String as utf-32
#+utf-32-strings
(progn
  (defmethod utf-32-p ((unicode string)) t)

  (defmethod unicode-length ((unicode string))
    (length unicode))

  (defmethod u32ref ((unicode string) index)
    (char-code (char unicode index)))

  (defmethod (setf u32ref) (code-unit (unicode string) index)
    (setf (char unicode index) (code-char code-unit))
    code-unit))


;; Convert unicode back to string

(defun unicode-to-string (unicode)
  (let ((string (make-string (unicode-length-for "" unicode)))
        (index 0))
    (do-code-points (code-point unicode)
      (setf index (set-code-point string index code-point)))
    string))

;; utf-8 unicode

(defstruct %utf-8
  (data nil :type (vector utf-8-code-unit)))

(defmethod make-load-form ((unicode %utf-8) &optional environment)
  (declare (ignore environment))
  `(make-%utf-8 :data ,(%utf-8-data unicode)))

(defun make-utf-8 (count)
  (make-%utf-8 :data (make-array count :element-type 'utf-8-code-unit)))

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

(defun make-utf-16 (count)
  (make-%utf-16 :data (make-array count :element-type 'utf-16-code-unit)))

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

(defun make-utf-32 (count)
  (make-%utf-32 :data (make-array count :element-type 'utf-32-code-unit)))

(defmethod utf-32-p ((unicode %utf-32)) t)

(defmethod unicode-length ((unicode %utf-32))
  (length (%utf-32-data unicode)))

(defmethod u32ref ((unicode %utf-32) index)
  (aref (%utf-32-data unicode) index))

(defmethod (setf u32ref) (code-unit (unicode %utf-32) index)
  (setf (aref (%utf-32-data unicode) index) code-unit))

;; unicode constructors

(defun make-unicode (format &rest data)
  (with-transform-error-restarts
    (let* ((length (loop for thing in data
                         summing (etypecase thing
                                   (unicode
                                    (unicode-length-for format thing))
                                   (integer
                                    (case format
                                      (utf-8 (check-type thing utf-8-code-unit))
                                      (utf-16 (check-type thing utf-16-code-unit))
                                      (utf-32 (check-type thing utf-32-code-unit))
                                      (string (check-type thing string-code-unit)))
                                    1))))
           (unicode (case format
                      (utf-8 (make-utf-8 length))
                      (utf-16 (make-utf-16 length))
                      (utf-32 (make-utf-32 length))
                      (string (make-string length)))))
      (loop with index = 0
            for elt in data
            do (etypecase elt
                 (unicode
                  (do-code-points (code-point elt)
                    (setf index (set-code-point unicode index code-point))))
                 (integer
                  (etypecase unicode
                    (utf-8 (setf (u8ref unicode index) elt))
                    (utf-16 (setf (u16ref unicode index) elt))
                    (utf-32 (setf (u32ref unicode index) elt)))
                  (incf index))))
      unicode)))

(defun utf-8 (&rest data)
  (apply #'make-unicode 'utf-8 data))

(defun utf-16 (&rest data)
  (apply #'make-unicode 'utf-16 data))

(defun utf-32 (&rest data)
  (apply #'make-unicode 'utf-32 data))

(defun unicode-string (&rest data)
  (apply #'make-unicode 'string data))


;; printing unicode readable


(defun print-unicode-code-units (stream type code-units digits)
  (format stream "(~S" type)
  (loop for byte across code-units
        do (format stream " #x~v,'0X" digits byte))
  (princ ")" stream))

(defmethod print-object ((unicode %utf-8) stream)
  (print-unicode-code-units stream 'utf-8 (%utf-8-data unicode) 2))

(defmethod print-object ((unicode %utf-16) stream)
  (print-unicode-code-units stream 'utf-16 (%utf-16-data unicode) 4))

(defmethod print-object ((unicode %utf-32) stream)
  (print-unicode-code-units stream 'utf-32 (%utf-32-data unicode) 4))
