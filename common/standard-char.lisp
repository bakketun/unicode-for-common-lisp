(in-package #:unicode-common)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +newline-code-point+ #x0D)
  (defconstant +space-code-point+   #x20)
  (defconstant +standard-char-code-point-limit+ 128)
  (defparameter +semi-standard-characters+
    (loop :for (name . code-point) :in `(("Backspace" . #x08)
                                         ("Tab"       . #x09)
                                         ("Linefeed"  . #x0A)
                                         ("Page"      . #x0C)
                                         ("Return"    . ,+newline-code-point+)
                                         ("Rubout"    . #x7F))
          :for char := (name-char name)
          :when char
            :collect (cons char code-point))
    "Assoc table of (char . code-point) semi standard characters supported by this implementation")


  (deftype semi-standard-char ()
    `(member ,@(mapcar #'car +semi-standard-characters+)))


  (deftype semi-and-standard-char()
    '(or standard-char semi-standard-char))


  (defparameter +graphic-standard-chars+
    " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~")


  (defparameter +code-point-to-char+
    (let ((map (make-array +standard-char-code-point-limit+ :initial-element nil)))
      (setf (svref map +newline-code-point+) #\Newline)
      (replace map +graphic-standard-chars+ :start1 +space-code-point+)
      (loop :for (char . code-point) :in +semi-standard-characters+
            :do (setf (svref map code-point) char))
      map)))


(defconstant +standard-char-code-limt+ (1+ (reduce #'max (remove nil +code-point-to-char+) :key #'char-code)))


(defparameter +char-code-to-code-point+
  (let ((map (make-array +standard-char-code-limt+ :initial-element nil)))
    (loop :for code-point :from 0
          :for char :across +code-point-to-char+
          :when char
            :do (setf (svref map (char-code char)) code-point))
    map))


(defun semi-and-standard-char-code-point (char)
  (check-type char semi-and-standard-char)
  (svref +char-code-to-code-point+ (char-code char)))


(deftype semi-and-standard-char-code-point ()
  `(or ,@(loop :with start
               :for (this next) :on (sort (coerce (remove nil +char-code-to-code-point+) 'list) #'<)
               :if (eql next (1+ this))
                 :do (unless start
                       (setf start this))
               :else
                 :collect `(integer ,start ,this)
                 :and :do (setf start nil))))


(defun code-point-semi-and-standard-char (code-point)
  (check-type code-point semi-and-standard-char-code-point)
  (svref +code-point-to-char+ code-point))
