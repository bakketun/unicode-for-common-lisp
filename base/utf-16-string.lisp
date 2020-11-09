(in-package #:unicode-base)


(defclass utf-16-string (code-unit-string)
  ()
  (:documentation "A code-unit-string in UTF-16 encoding form."))


(defclass standard-utf-16-string (utf-16-string byte-vector-code-unit-string)
  ((code-units :type '(vector (unsigned-byte 16)))))


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
