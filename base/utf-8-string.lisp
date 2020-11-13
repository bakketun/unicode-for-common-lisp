(in-package #:unicode-base)


(defclass utf-8-string (code-unit-string)
  ()
  (:documentation "A code-unit-string in UTF-8 encoding form."))


(defgeneric utf-8-string (x)
  (:documentation "Converts x into utf-8-string if not already so.")
  (:method ((x utf-8-string)) x))


(defgeneric utf-8-curef (custring index)
  (:documentation "Get code-unit at index in UTF-8 encoded custring."))


(defclass standard-utf-8-string (utf-8-string)
  ((code-units :type (vector (unsigned-byte 8))
               :initarg :code-units)))


(defmethod generic-culength ((custring standard-utf-8-string))
  (length (slot-value custring 'code-units)))


(defmethod utf-8-curef ((custring standard-utf-8-string) index)
  (aref (slot-value custring 'code-units) index))


(defmethod utf-8-string (x)
  (make-instance 'standard-utf-8-string :code-units (utf-8-code-unit-vector x)))


(defmethod generic-cuencoding ((custring utf-8-string))
  :utf-8)


(defmethod generic-code-point-at ((encoding (eql :utf-8)) custring index start end)
  "Works for any object that implements utf-8-curef."
  (let (code-point subsequence-start subsequence-end errorp)
    (code-point-decode-utf-8 :backtrackp t
                             :index-form index
                             :start-form start
                             :end-form end
                             :ref-form `(utf-8-curef custring ,*)
                             :code-point-place code-point
                             :start-place subsequence-start
                             :end-place subsequence-end
                             :errorp-place errorp)
    (values code-point subsequence-end subsequence-start errorp)))
