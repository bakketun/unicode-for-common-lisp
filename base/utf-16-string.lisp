(in-package #:unicode-base)


(defclass utf-16-string (code-unit-string)
  ()
  (:documentation "A code-unit-string in UTF-16 encoding form."))


(defgeneric utf-16-string (x)
  (:documentation "Converts x into utf-16-string if not already so.")
  (:method ((x utf-16-string)) x))


(defgeneric utf-16-curef (custring index)
  (:documentation "Get code-unit at index in UTF-16 encoded custring."))


(defclass standard-utf-16-string (utf-16-string)
  ((code-units :type (vector (unsigned-byte 16))
               :initarg :code-units)))


(defmethod generic-culength ((custring standard-utf-16-string))
  (length (slot-value custring 'code-units)))


(defmethod utf-16-curef ((custring standard-utf-16-string) index)
  (aref (slot-value custring 'code-units) index))


(defmethod utf-16-string (x)
  (make-instance 'standard-utf-16-string :code-units (utf-16-code-unit-vector x)))


(defmethod generic-cuencoding ((custring utf-16-string))
  :utf-16)

(defmethod generic-code-point-at ((encoding (eql :utf-16)) custring index start end)
  "Works for any object that implements utf-16-curef."
  ;; Todo check start, end
  (let ((code-point (utf-16-curef custring index))
        (next (1+ index))
        (start index)
        (error nil))
    (flet ((curef? (index)
             (when (< 0 index (culength custring))
               (utf-16-curef custring index)))
           (decode (leading trailing)
             (logior (ash  (1+ (ldb (byte  4 6) leading)) 16)
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


(defun code-point-utf-16-encode (code-point)
  "Returns three values: code unit count, code unit 0, code unit 1."
  (check-type code-point scalar-value)
  (etypecase code-point
    (bmp-code-point
     (values 1 code-point))
    (t
     (values 2
             (logior +first-high-surrogate+
                       (1- (ldb (byte 5 16) code-point))
                       (ldb (byte 6 10) code-point))
             (logior +first-low-surrogate+
                     (ldb (byte 10 0) code-point))))))
