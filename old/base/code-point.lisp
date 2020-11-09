
#-(or code-point-is-integer code-point-is-character code-point-is-class)
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;(pushnew :code-point-is-integer *features*)
  ;(pushnew :code-point-is-character *features*)
  (pushnew :code-point-is-class *features*)
  )


(deftype code-point-code ()
  "The subtype of integer that can represent every code point."
  '(integer 0 #x10FFFF))


(defstruct (standard-code-point
            (:constructor make-standard-code-point (code)))
  "A class only used to represent code points."
  (code nil :type code-point-code :read-only t))


(defgeneric generic-code-point (object)
  (:documentation
   "Allows the function code-point to be extended.")
  (:method (object)
    nil))


(deftype code-point ()
  #+code-point-is-class
  'standard-code-point
  #+code-point-is-integer
  'code-point-code
  #+code-point-is-character
  '(and character
    (not (member #\Newline))))


(defun code-point (object)
  (typecase object
    (code-point
     object)
    (code-point-code
     #+code-point-is-class
     (make-standard-code-point object)
     #+code-point-is-integer
     object
     #+code-point-is-character
     (code-char object))
    (t
     (generic-code-point object))))


(defun code-point-code (object)
  (typecase object
    (null
     nil)
    (code-point-code
     object)
    (standard-code-point
     (standard-code-point-code object))
    #+code-point-is-character
    (character
     (char-code object))
    (t
     (code-point-code (code-point object)))))


(deftype surrogate-code-point-code ()
  '(integer #xD800 #xDFFF))


(defun surrogate-code-point-p (object)
  (when (typep (code-point-code object) 'surrogate-code-point-code)
    object))


(deftype surrogate-code-point ()
  '(and code-point
    (satisfies surrogate-code-point-p)))


(deftype scalar-value ()
  '(and code-point-code
    (not surrogate-code-point-code)))


(defun scalar-value-p (object)
  (let ((code (code-point-code object)))
    (when (typep code 'scalar-value)
      code)))


(defun scalar-value (object)
  (let ((code-point-code (code-point-code object)))
    (check-type code-point-code scalar-value)
    code-point-code))




(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +semi-standard-char-name-map+
    (loop :for (name . code) :in '(("Backspace" . 8)
                                   ("Tab" . 9)
                                   ("Page" . 12)
                                   ("Return" . 13)
                                   ("Space" . 32)
                                   ("Rubout" . 127))
          :for char := (name-char name)
          :when char
            :collect (cons char code))))


(deftype semi-standard-char ()
  `(member ,@(mapcar #'car +semi-standard-char-name-map+)))


(defparameter +graphic-and-space-standard-chars+
  " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~")


(defparameter +map-char-to-code-point+
  (make-hash-table))


(defparameter +map-code-point-code-to-char+
  (coerce
   (loop :for code-point-code :from 0 :to 127
         :for char := (typecase code-point-code
                        ((integer 32 126)
                         (char +graphic-and-space-standard-chars+ (- code-point-code 32)))
                        (t
                         (car (find code-point-code +semi-standard-char-name-map+ :key #'cdr))))
         :when char
           :do (setf (gethash char +map-char-to-code-point+) (code-point code-point-code))
         :collect char)
   'vector))


(defun char-code-point (char)
  (values (gethash char +map-char-to-code-point+)))


(defun code-point-char (code-point)
  (let ((code (code-point-code code-point)))
    (when code
      (svref +map-code-point-code-to-char+ code))))


(defmethod generic-code-point ((char character))
  (char-code-point char))
