(in-package #:unicode-base)


(defclass code-unit-string ()
  ()
  (:documentation "A 'string' of code units encoding Unicode text.

The code units can be dived into subsequences where each subsequence
is either well-formed and encodes a single scalar value code point or
ill-formed and encodes the replacement character U+FFFD.
"))


(defun cuencoding (custring)
  "One of :UTF-8, :UTF-16, :UTF-32 or STANDARD-CHAR"
  (generic-cuencoding custring))


(defun culength (custring)
  "Number of code units in the code-unit-string."
  (generic-culength custring))


(defun code-point-at (custring index &key start end encoding)
  "Returns the code-point (scalar value really) at index.

Values returned are:

code-point - The code point at location or #xFFFD (REPLACEMENT
CHARACTER) if there was a decoding error.

next-index - The index of the next code-point.

start - The start index of the code point. Will be less than index
when it points in the middle of a code unit sequence.

ill-formed-p - True if the decoded subsequence was ill-formed."
  (generic-code-point-at (or encoding (cuencoding custring))
                         custring
                         index
                         (or start 0)
                         (or end (culength custring))))


;; Required functions of the code-unit-string protocol
(defgeneric generic-culength (custring))
(defgeneric generic-code-point-at (encoding custring index start end))
(defgeneric generic-cuencoding (custring))

;; Derived functions of the code-unit-string protocol

(defgeneric code-point-count (custring)
  (:documentation "Returns number of code points (scalar values) in string.")
  (:method (custring)
    (loop :with end := (culength custring)
          :for index := 0 :then (nth-value 1 (code-point-at custring index :start index))
          :while (< index end)
          :count t)))


(defgeneric map-code-points (function custring)
  (:documentation "Calls function for each code point in custring")
  (:method (function custring)
    (loop :with end := (culength custring)
          :with next
          :with code-point
          :for index := 0 :then next
          :while (< index end)
          :do (multiple-value-setq (code-point next)
                (code-point-at custring index))
          :do (funcall function code-point))))


(defgeneric utf-8-code-unit-vector (custring)
  (:documentation "UTF-8 encodes the custring as subtype of (vector (unsigned-byte 8))")
  (:method (custring)
    (let ((length 0))
      (map-code-points (lambda (code-point)
                         (incf length (code-point-encode-utf-8 code-point)))
                       custring)
      (let ((vector (make-array length :element-type '(unsigned-byte 8)))
            (index 0))
        (map-code-points (lambda (code-point)
                           (multiple-value-bind (size b0 b1 b2 b3)
                               (code-point-encode-utf-8 code-point)
                             (setf                    (aref vector index) b0)   (incf index)
                             (when (< 1 size)   (setf (aref vector index) b1)   (incf index))
                             (when (< 2 size)   (setf (aref vector index) b2)   (incf index))
                             (when (< 3 size)   (setf (aref vector index) b3)   (incf index))))
                         custring)
        vector))))


(defgeneric utf-16-code-unit-vector (custring)
  (:documentation "UTF-16 encodes the custring as subtype of (vector (unsigned-byte 16))")
  (:method (custring)
    (let ((length 0))
      (map-code-points (lambda (code-point)
                         (incf length (code-point-encode-utf-16 code-point)))
                       custring)
      (let ((vector (make-array length :element-type '(unsigned-byte 16)))
            (index 0))
        (map-code-points (lambda (code-point)
                           (multiple-value-bind (size cu0 cu1)
                               (code-point-encode-utf-16 code-point)
                             (setf                    (aref vector index) cu0)   (incf index)
                             (when (< 1 size)   (setf (aref vector index) cu1)   (incf index))))
                         custring)
        vector))))


(defgeneric utf-32-code-unit-vector (thing)
  (:documentation "UTF-32 encodes the custring as subtype of (vector (unsigned-byte 32))")
  (:method (custring)
    (let ((code-points (make-array (code-point-count custring) :element-type 'scalar-value))
          (index 0))
      (map-code-points (lambda (code-point)
                         (setf (aref code-points index) code-point)
                         (incf index))
                       custring)
      code-points)))
