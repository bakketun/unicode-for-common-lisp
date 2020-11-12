(in-package #:unicode-example)

(defmacro show (&whole whole &body body)
  `(with-simple-restart (continue "Skip example")
     (format t "~&~%~S~&" ',whole)
     (format t "~&~80@{-~}~{~&~S~}~&~:*~80@{=~}"
             (multiple-value-list (progn ,@body)))))

(defun run ()

  (show (custring "Hello World"))

  (show (culength (custring "Hello World")))

  (show (code-point-count (custring "Blåbærsyltetøy")))


  (show
    "Using string as designator for custring"
    (code-point-count "Blåbærsyltetøy"))

  (show (utf-32-code-unit-vector "Blåbærsyltetøy"))
  (show (utf-16-code-unit-vector "Blåbærsyltetøy"))
  (show (utf-8-code-unit-vector "Blåbærsyltetøy"))


  (show
    (string* (make-instance 'standard-utf-8-string :code-units #(65 66 67))))

  (show
    (string* (make-instance 'standard-utf-8-string :code-units #(195 165))))


  (show
    (code-point-at (make-instance 'standard-utf-8-string :code-units #(165)) 0))


  (show (string* (make-instance 'standard-utf-8-string :code-units #(#xf0 #x90 #x8c #x82))))
  (show (string* (make-instance 'standard-utf-16-string :code-units #(#xd800 #xdf02))))
  (show (string* (make-instance 'standard-utf-32-string :code-units #(#x00010302))))

  (show (reduce #'+ '(65 #x10302) :key #'code-point-utf-8-encode))

  (show (reduce #'+ '(65 #x10302) :key #'code-point-utf-16-encode))

  (string* 'fooo)
  )
