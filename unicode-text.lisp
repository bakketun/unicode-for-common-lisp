(in-package #:unicode)


(defgeneric code-point-at (unicode index))
(defgeneric unicode-length (unicode))

(defun next-code-point (unicode index)
  (nth-value 1 (code-point-at unicode index)))


(defun code-point-count (unicode)
  (loop for index = 0 then (next-code-point unicode index)
        while (< index (unicode-length unicode))
        count t))


(defun map-code-points (function unicode)
  (loop with code-point
        with index = 0
        while (< index (unicode-length unicode))
        do (multiple-value-setq (code-point index) (code-point-at unicode index))
        do (funcall function code-point)))


;;; utf-32 string as unicode


(defmethod unicode-length ((unicode string))
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


(defclass unicode ()
  ((data :initarg :data :accessor unicode-data)))


(defmethod unicode-length ((unicode unicode))
  (length (slot-value unicode 'data)))


;;; utf-32


(defclass unicode-utf-32 (unicode)
  ())


(defmethod code-point-at ((unicode unicode-utf-32) index)
  (values (aref (unicode-data unicode) index)
          (1+ index)))


(defun to-utf-32 (unicode)
  (let ((data (make-array (code-point-count unicode) :element-type '(unsigned-byte 32)))
        (index 0))
    (map-code-points (lambda (code-point)
                       (setf (aref data index) code-point)
                       (incf index))
                     unicode)
    (make-instance 'unicode-utf-32 :data data)))


;;; utf-16


(defclass unicode-utf-16 (unicode)
  ())


(defmethod code-point-at ((unicode unicode-utf-16) index)
  (let ((lead (aref (unicode-data unicode) index)))
    (if (<= #xD800 lead #xDBFF)
        (let ((tail (aref (unicode-data unicode) (1+ index))))
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
  (let ((data (make-array (utf-16-length unicode) :element-type '(unsigned-byte 16)))
        (index 0))
    (map-code-points (lambda (code-point)
                       (cond ((< code-point #xFFFF)
                              (setf (aref data index) code-point)
                              (incf index 1))
                             (t
                              (let ((tmp (- code-point #x010000)))
                                (setf (aref data index) (+ #xD800 (ldb (byte 10 10) tmp)))
                                (setf (aref data (1+ index)) (+ #xDC00 (ldb (byte 10 0) tmp))))
                              (incf index 2))
                             ))
                     unicode)
    (make-instance 'unicode-utf-16 :data data)))


;;; utf-8


(defclass unicode-utf-8 (unicode)
  ())


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
           (ldb (byte 6 0) (aref (unicode-data unicode) (+ n index)))))
    (let ((leader (aref (unicode-data unicode) index)))
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
    (make-instance 'unicode-utf-8 :data data)))
