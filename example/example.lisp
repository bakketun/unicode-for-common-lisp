(in-package #:unicode-example)

(defmacro show (&whole whole &body body)
  `(with-simple-restart (continue "Skip example")
     (format t "~&~%~S~&" ',whole)
     (format t "~&~80@{-~}~{~&~S~}~&~:*~80@{=~}"
             (multiple-value-list (progn ,@body)))))

(defun run ()

  (show (culength "Hello World"))

  (show (code-point-at "Blåbærsyltetøy" 0))

  (show (code-point-count "Blåbærsyltetøy"))


  (show
    "Using string as designator for custring"
    (code-point-count "Blåbærsyltetøy"))

  (show (utf-32-code-unit-vector "Blåbærsyltetøy"))
  (show (utf-16-code-unit-vector "Blåbærsyltetøy"))
  (show (utf-8-code-unit-vector "Blåbærsyltetøy"))


  (show
    (unicode-to-string (make-instance 'standard-utf-8-string :code-units #(65 66 67))))

  (show
    (unicode-to-string (make-instance 'standard-utf-8-string :code-units #(195 165))))


  (show
    (code-point-at (make-instance 'standard-utf-8-string :code-units #(165)) 0))


  (show (unicode-to-string (make-instance 'standard-utf-8-string :code-units #(#xf0 #x90 #x8c #x82))))
  (show (unicode-to-string (make-instance 'standard-utf-16-string :code-units #(#xd800 #xdf02))))
  (show (unicode-to-string (make-instance 'standard-utf-32-string :code-units #(#x00010302))))


  (show (code-point-at (make-instance 'standard-utf-8-string :code-units #(195 165 165)) 2))
  )
