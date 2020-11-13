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
  (let (code-point subsequence-start subsequence-end errorp)
    (code-point-decode-utf-16 :backtrackp t
                              :index-form index
                              :start-form start
                              :end-form end
                              :ref-form `(utf-16-curef custring ,*)
                              :code-point-place code-point
                              :start-place subsequence-start
                              :end-place subsequence-end
                              :errorp-place errorp)
    (values code-point subsequence-end subsequence-start errorp)))
