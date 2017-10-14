(in-package #:unicode)


(deftype code-point ()
  '(integer #x0 #x10FFFF))


(deftype unicode-scalar ()
  '(or (integer 0 #xD7FF)
    (integer #xE000 #x10FFFF)))


(defgeneric code-point-at (unicode index))


(declaim (inline unicode-length))

;;(defgeneric unicode-length (unicode))
(defun unicode-length (unicode)
  (declare (inline unicode-length) (optimize (speed 3)))
  (etypecase unicode
    (string (length unicode))
    (unicode-utf-8 (length (unicode-utf-8-data unicode)))
    (unicode-utf-16 (length (unicode-utf-16-data unicode)))
    (unicode-utf-32 (length (unicode-utf-32-data unicode)))))


(defun next-code-point (unicode index)
  (nth-value 1 (code-point-at unicode index)))


(defun code-point-count (unicode)
  (loop for index = 0 then (next-code-point unicode index)
        while (< index (unicode-length unicode))
        count t))


'(defun map-code-points (function unicode)
  (declare (inline unicode-length)
           (optimize (speed 3))
           (function function))
  (loop with code-point
        with index fixnum = 0
        while (< index (unicode-length unicode))
        do (multiple-value-setq (code-point index) (code-point-at unicode index))
        do (funcall function code-point)))


(defmacro do-code-points ((var unicode) &body body)
  (let ((index (gensym "INDEX"))
        (unicode-var (gensym "UNICODE-VAR")))
    `(loop with ,var of-type code-point
           with ,index fixnum = 0
           with ,unicode-var = ,unicode
           while (< ,index (unicode-length ,unicode-var))
           do (multiple-value-setq (,var ,index) (code-point-at ,unicode-var ,index))
           do (progn ,@body))))


(defun map-code-points (function unicode)
  (declare (optimize (speed 3))
           (function function))
  (do-code-points (code-point unicode)
    (funcall function code-point)))


;;; utf-32 string as unicode


'(defmethod unicode-length ((unicode string))
  (length unicode))


(defmethod code-point-at ((unicode string) index)
  (values (char-code (char unicode index))
          (1+ index)))


(defun unicode-to-string (unicode)
  (let ((string (make-string (code-point-count unicode)))
        (index 0))
    (map-code-points (lambda (code-point)
                       (setf (char string index) (code-char code-point))
                       (incf index))
                     unicode)
    string))


;;; generic unicode type


'(defstruct unicode
  (data #() :type vector))

'(defclass unicode ()
  ((data :initarg :data :accessor unicode-data :type 'vector)))


'(defmethod unicode-length ((unicode unicode))
  (length (slot-value unicode 'data)))


;;; utf-32


'(defclass unicode-utf-32 (unicode)
  ())


(defstruct unicode-utf-32
  (data nil :type (vector (unsigned-byte 32))))


(defmethod code-point-at ((unicode unicode-utf-32) index)
  (values (aref (unicode-utf-32-data unicode) index)
          (1+ index)))


(defun to-utf-32 (unicode)
  (let ((data (make-array (code-point-count unicode) :element-type '(unsigned-byte 32)))
        (index 0))
    (map-code-points (lambda (code-point)
                       (setf (aref data index) code-point)
                       (incf index))
                     unicode)
    (make-unicode-utf-32 :data data)))


;;; utf-16


'(defclass unicode-utf-16 (unicode)
  ())


(defstruct unicode-utf-16
  (data nil :type (vector (unsigned-byte 16))))


(defmethod code-point-at ((unicode unicode-utf-16) index)
  (let ((lead (aref (unicode-utf-16-data unicode) index)))
    (if (<= #xD800 lead #xDBFF)
        (let ((tail (aref (unicode-utf-16-data unicode) (1+ index))))
          (values (+ #x10000
                     (ash (- lead #xD800) 10)
                     (- tail #xDC00))
                  (+ 2 index)))
        (values lead (1+ index)))))


(defun utf-16-length (unicode)
  (let ((length 0))
    (map-code-points (lambda (code-point)
                       (if (<= code-point #xFFFF)
                           (incf length 1)
                           (incf length 2)))
                     unicode)
    length))


(defun to-utf-16 (unicode)
  (declare (optimize (speed 3)))
  (let ((data (make-array (utf-16-length unicode) :element-type '(unsigned-byte 16)))
        (index 0))
    (do-code-points (code-point unicode)
      (cond ((< code-point #xFFFF)
             (setf (aref data index) code-point)
             (incf index 1))
            (t
             (setf (aref data index) (+ #xD7C0 (ldb (byte 10 10) code-point)))
             (setf (aref data (1+ index)) (+ #xDC00 (ldb (byte 10 0) code-point)))
             (incf index 2))))
    (make-unicode-utf-16 :data data)))


;;; utf-8


'(defclass unicode-utf-8 (unicode)
  ())


(defstruct unicode-utf-8
  (data nil :type (vector (unsigned-byte 8))))


(defun utf-8-length (unicode)
  (let ((length 0))
    (map-code-points (lambda (code-point)
                       (cond ((< code-point #x80)
                              (incf length 1))
                             ((< code-point #x800)
                              (incf length 2))
                             ((< code-point #x10000)
                              (incf length 3))
                             (t
                              (incf length 4))))
                     unicode)
    length))


(defmethod code-point-at ((unicode unicode-utf-8) index)
  (flet ((cbyte (n)
           (ldb (byte 6 0) (aref (unicode-utf-8-data unicode) (+ n index)))))
    (let ((leader (aref (unicode-utf-8-data unicode) index)))
      (cond ((< leader #x80)
             (values leader (1+ index)))
            ((= #b110 (ldb (byte 3 5) leader))
             (values (logxor (ash (ldb (byte 5 0) leader) 6)
                             (cbyte 1))
                     (+ 2 index)))
            ((= #b110 (ldb (byte 3 5) leader))
             (values (logxor (ash (ldb (byte 4 0) leader) 12)
                             (ash (cbyte 1) 6)
                             (cbyte 2))
                     (+ 3 index)))
            ((= #b110 (ldb (byte 3 5) leader))
             (values (logxor (ash (ldb (byte 3 0) leader) 18)
                             (ash (cbyte 1) 12)
                             (ash (cbyte 2) 6)
                             (cbyte 3))
                     (+ 4 index)))))))


(defun code-point-to-utf-8** (code-point )
  (cond ((< code-point #x80)
         (values 1 code-point 0 0 0))
        ((< code-point #x800)
         (values 2
                 (logxor #b11000000 (ldb (byte 5 6) code-point))
                 (logxor #b10000000 (ldb (byte 6 0) code-point))
                 0
                 0))
        ((< code-point #x10000)
         (values 3
                 (logxor #b11100000 (ldb (byte 4 12) code-point))
                 (logxor #b10000000 (ldb (byte 6 6) code-point))
                 (logxor #b10000000 (ldb (byte 6 0) code-point))
                 0))
        (t
         (values 4
                 (logxor #b11110000 (ldb (byte 3 18) code-point))
                 (logxor #b10000000 (ldb (byte 6 12) code-point))
                 (logxor #b10000000 (ldb (byte 6 6) code-point))
                 (logxor #b10000000 (ldb (byte 6 0) code-point))))))


(defun code-point-to-utf-8 (code-point buffer start)
  (cond ((< code-point #x80)
         (setf (aref buffer start) code-point)
         (1+ start))
        ((< code-point #x800)
         (setf (aref buffer start)
               (logxor #b11000000 (ldb (byte 5 6) code-point)))
         (setf (aref buffer (+ 1 start))
               (logxor #b10000000 (ldb (byte 6 0) code-point)))
         (+ 2 start))
        ((< code-point #x10000)
         (setf (aref buffer start)
               (logxor #b11100000 (ldb (byte 4 12) code-point)))
         (setf (aref buffer (+ 1 start))
               (logxor #b10000000 (ldb (byte 6 6) code-point)))
         (setf (aref buffer (+ 2 start))
               (logxor #b10000000 (ldb (byte 6 0) code-point)))
         (+ 3 start))
        (t
         (setf (aref buffer start)
               (logxor #b11110000 (ldb (byte 3 18) code-point)))
         (setf (aref buffer (+ 1 start))
               (logxor #b10000000 (ldb (byte 6 12) code-point)))
         (setf (aref buffer (+ 2 start))
               (logxor #b10000000 (ldb (byte 6 6) code-point)))
         (setf (aref buffer (+ 3 start))
               (logxor #b10000000 (ldb (byte 6 0) code-point)))
         (+ 4 start))))


(defun to-utf-8 (unicode)
  (let ((data (make-array (utf-8-length unicode) :element-type '(unsigned-byte 8)))
        (index 0))
    (map-code-points (lambda (code-point)
                       (setf index (code-point-to-utf-8 code-point data index)))
                     unicode)
    (make-unicode-utf-8 :data data)))
