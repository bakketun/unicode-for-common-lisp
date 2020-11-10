(in-package #:unicode-base)


(defclass code-unit-string ()
  ()
  (:documentation "A string of code units encoding Unicode text."))


(defgeneric custring (x)
  (:documentation "Convert x into a code-unit-string if not already so."))


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


(defgeneric code-point-count (custring)
  (:documentation "Returns number of code points (scalar values) in string."))


(defmethod code-point-count ((custring code-unit-string))
  (loop :with end := (culength custring)
        :for index := 0 :then (nth-value 1 (code-point-at custring index))
        :while (< index end)
        :count t))


(defmethod map-code-points (function (custring code-unit-string))
  (loop :with end := (culength custring)
        :with next
        :with code-point
        :for index := 0 :then next
        :while (< index end)
        :do (multiple-value-setq (code-point next)
              (code-point-at custring index))
        :do (funcall function code-point)))


(defgeneric utf-8-code-unit-vector (thing)
  (:method (thing)
    (utf-8-code-unit-vector (custring thing)))
  (:method ((custring code-unit-string))
    (let ((length 0))
      (map-code-points (lambda (code-point)
                         (incf length (code-point-utf-8-encode code-point)))
                       custring)
      (let ((vector (make-array length :element-type '(unsigned-byte 8)))
            (index 0))
        (map-code-points (lambda (code-point)
                           (multiple-value-bind (size b0 b1 b2 b3)
                               (code-point-utf-8-encode code-point)
                             (setf                    (aref vector index) b0)   (incf index)
                             (when (< 1 size)   (setf (aref vector index) b1)   (incf index))
                             (when (< 2 size)   (setf (aref vector index) b2)   (incf index))
                             (when (< 3 size)   (setf (aref vector index) b3)   (incf index))))
                         custring)
        vector))))


(defgeneric utf-16-code-unit-vector (thing)
  (:method (thing)
    (utf-16-code-unit-vector (custring thing)))
  (:method ((custring code-unit-string))
    (let ((length 0))
      (map-code-points (lambda (code-point)
                         (check-type code-point scalar-value)
                         (incf length (etypecase code-point
                                        (bmp-code-point 1)
                                        (t 2))))
                       custring)
      (let ((vector (make-array length :element-type '(unsigned-byte 16)))
            (index 0))
        (map-code-points (lambda (code-point)
                           (etypecase code-point
                             (bmp-code-point
                              (setf (aref vector index) code-point)
                              (incf index))
                             (t
                              (setf (aref vector index) (logior +first-high-surrogate+
                                                                (1- (ldb (byte 5 16) code-point))
                                                                (ldb (byte 6 10) code-point)))
                              (incf index)
                              (setf (aref vector index) (logior +first-low-surrogate+
                                                                (ldb (byte 10 0) code-point)))
                              (incf index))))
                         custring)
        vector))))


(defgeneric utf-32-code-unit-vector (thing)
  (:method (thing)
    (utf-32-code-unit-vector (custring thing)))
  (:method ((custring code-unit-string))
    (let ((code-points (make-array (code-point-count custring) :element-type 'scalar-value))
          (index 0))
      (map-code-points (lambda (code-point)
                         (setf (aref code-points index) code-point)
                         (incf index))
                       custring)
      code-points)))
