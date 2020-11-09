(defmacro defconstant-once (name value &optional documentation)
  `(defconstant ,name
     (if (boundp ',name)
         ,name
         ,value)
     ,documentation))


(deftype code-point-int ()
  "The subtype of integer that can represent every code point."
  '(integer 0 #x10FFFF))

(deftype surrogate-code-point-int ()
  "The subtype of integer that can represent surrogate code points."
  '(integer #xD800 #xDFFF))

(deftype scalar-value-int ()
  "The subtype of integer that can represent every scalar value."
  ()
  '(and
    code-point-int
    (not surrogate-code-point-int)))


(defclass text-encoding ()
  ()
  (:documentation "Defines what sequence of scalar values a text element encodes."))


(defclass text-element ()
  ()
  (:documentation "The basic unit for encoding text. A text element is
  used to encode a sequence of zero or more scalar values.

The are two distinct classes of text elements:

Text encoding agnostic - Always encodes the same sequence of scalar values.

Text encoding dependant - The text encoding defines which scalar
values the text element encodes.

Text element objects are immutable."))

(defgeneric text-element-length (text-element)
  (:documentation "Number of scalar values the text element encodes. Returns NIL for encoding dependant text elements")
  (:method ((text-element text-element)) nil))


(defclass empty-text-element (text-element)
  ((length :initform 0 :reader text-element-length))
  (:documentation "A text element of class empty-text-element always represent an empty sequence of scalar values."))

(defconstant-once +empty-text-element+
    (make-instance 'empty-text-element)
  "The cannonical empty text element.")


(defclass code-point (text-element)
  ((int :type code-point-int
        :initarg :int
        :reader code-point-int))
  (:documentation "The supertype of scalar-value and surrogate-code-point"))

(defmethod code-point-int ((int integer))
  (check-type int code-point-int)
  int)

(defclass scalar-value (code-point)
  ((int :type scalar-value-int)
   (length :initform 1 :reader text-element-length))
  (:documentation "Text elements of type scalar-value always encodes a single scalar value."))

(defconstant-once +replacement-character+
    (make-instance 'code-point :int #xFFFD)
  "U+FFFD REPLACEMENT CHARACTER as a text-element")

(defconstant-once +object-replacement-character+
    (make-instance 'code-point :int #xFFFC)
  "U+FFFC OBJECT REPLACEMENT CHARACTER as a text-element")


(defclass char-text-element (text-element)
  ((char :type 'character
         :initarg :char
         :reader char-text-element-char)
   (length :reader text-element-length))
  (:documentation "A single cl:character as a text element. The scalar
  values encoded by standard characters is fixed, except for Newline
  which is text encoding dependant."))

(defmethod shared-initialize ((elt char-text-element) slot-names &key char)
  (setf (slot-value elt 'length) (cond ((eql #\Newline char)
                                        nil)
                                       ((standard-char-p char)
                                        1)
                                       (t
                                        nil))))

(defconstant-once +newline-text-element+
    (make-instance 'char-text-element :char #\Newline)
  "The Newline character as a text-element")


(defclass encoding-error-text-element (text-element)
  ()
  (:documentation "A text element representing an encoding error."))


(defgeneric text-element-equal (x y)
  (:documentation "TODO Not sure about this.")
  (:method (x y)
    nil)
  (:method ((x code-point) (y code-point))
    (eql (code-point-int x) (code-point-int y)))
  (:method ((x empty-text-element) (y empty-text-element))
    t)
  (:method ((x char-text-element) (y char-text-element))
    (eql (char-text-element-char x) (char-text-element-char y))))



(defclass text ()
  ()
  (:documentation "Generic data type for Unicode text.

Text is a sequence of text elements encoding a sequence of scalar
values. Each text element encodes zero or more scalar values."))

(defclass immutable-text (text text-element)
  ()
  (:documentation "Instances of immutable text always encodes the exact same sequence of scalar values."))

(defclass flat-text (text)
  ()
  (:documentation "In flat text each element encodes either a single
  scalar value or no scalar value at all."))


;; The protocol for text element

;; Other possible text classes:
;;
;; text input stream
;; text output stream
;;
;; text buffer - something similar to buffer in Emacs?
;; Has an insertion point, selection, (overwrite flag?)
;; A stream is very similar to a text buffer.

(defclass utf-8 (flat-text)
  ()
  (:documentation "Generic data type for Unicode text encoded as UTF-8. Each element is of type (unsigned-byte 8)."))

(defclass utf-16 (flat-text)
  ()
  (:documentation "Generic data type for Unicode text encoded as UTF-16. Each element is of type (unsigned-byte 16)."))

(defclass utf-16le (flat-text)
  ()
  (:documentation "Generic data type for Unicode text encoded as UTF-16LE. Each element is of type (unsigned-byte 8)."))

(defclass utf-16be (flat-text)
  ()
  (:documentation "Generic data type for Unicode text encoded as UTF-16BE. Each element is of type (unsigned-byte 8)."))

(defclass utf-32 (flat-text)
  ()
  (:documentation "Generic data type for Unicode text encoded as UTF-16. Each element is of type (unsigned-byte 32)."))

(defclass utf-32le (flat-text)
  ()
  (:documentation "Generic data type for Unicode text encoded as UTF-32LE. Each element is of type (unsigned-byte 8)."))

(defclass utf-32be (flat-text)
  ()
  (:documentation "Generic data type for Unicode text encoded as UTF-32BE. Each element is of type (unsigned-byte 8)."))


(defmacro define-byte-vector-text-mixin (name bits)
  `(progn
     (defclass ,name ()
       ((bytes :initarg :bytes
               :type (vector (unsigned-byte ,bits))
               :initform ,(make-array 0 :element-type `(unsigned-byte ,bits)))))
     (defmethod shared-initialize ((text ,name) slot-names &key code-units)
       (when code-units
         (setf (slot-value text 'bytes) (coerce code-units '(vector (unsigned-byte ,bits))))))))

(define-byte-vector-text-mixin 8-bit-text 8)
(define-byte-vector-text-mixin 16-bit-text 16)
(define-byte-vector-text-mixin 32-bit-text 32)




(defclass standard-utf-8 (utf-8 8-bit-text) ())
(defclass standard-utf-16 (utf-16 16-bit-text) ())
(defclass standard-utf-16le (utf-16 8-bit-text) ())
(defclass standard-utf-16be (utf-16 8-bit-text) ())
(defclass standard-utf-32 (utf-32 32-bit-text) ())
(defclass standard-utf-32le (utf-32le 8-bit-text) ())
(defclass standard-utf-32be (utf-32be 8-bit-text) ())


;; Defaults are implenetation defined
(defvar *default-text-encoding* 'utf-32
  "Text objects will be of a subclass of this type by default.")

(defvar *default-utf-8-class* 'standard-utf-8
  "Text objects of subclass UTF-8 will be of this class by default.")

(defvar *default-utf-16-class* 'standard-utf-16
  "Text objects of subclass UTF-16 will be of this class by default.")

(defvar *default-utf-32-class* 'standard-utf-32
  "Text objects of subclass UTF-32 will be of this class by default.")


(defgeneric utf-32-code-point-at (text index start end))


(defmethod utf-32-code-point-at ((text standard-utf-32) index start end)
  (aref (slot-value text 'bytes) index))





(defun code-point-at (text index &key start end decode-error)
  "Return value of code point encoded at index.

Second value is next index after the code point.

Third value is the start index of the code point. If start is equal to
index, backtracking is disabled.

start and end defines the bounds for decoding a code point.

error specifies handling of decoding errors.
"
   )

(defun text (text-designator &key encoding)
  (etypecase text-designator
    (text
     ;; TODO check if text is of correct encoding
     text-designator)
    (sequence
     (make-instance *default-utf-32-class* :code-units text-designator))))
