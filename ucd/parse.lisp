(defparameter *ucd-directory* (merge-pathnames #p"data/Public/13.0.0/ucd/*.txt"
                                               (or *load-truename* *compile-file-truename*)))


(defun trim (string)
  (string-trim #(#\Space #\Tab) string))


(defun unhex (string &optional (start 0) (end (length string)))
  (parse-integer string :start start :end end :radix 16))


(defun parse-code-point (string)
  (let ((..pos (search ".." string)))
    (if ..pos
        (cons (unhex string 0 ..pos) (unhex string (+ 2 ..pos)))
        (unhex string))))


(defun load-data (filename)
  (with-open-file (in (merge-pathnames filename *ucd-directory*))
    (loop :for line := (read-line in nil)
          :while line
          :when (let* ((comment-start (position #\# line))
                          (comment (when comment-start (trim (subseq line (1+ comment-start)))))
                          (data-end (or comment-start (length line)))
                          (data (loop :for start := 0 :then (1+ end)
                                      :while (< start data-end)
                                      :for end := (or (position #\; line :start start :end data-end) data-end)
                                      :collect (trim (subseq line start end)))))
                     (when (or comment data)
                       (list data comment)))
            :collect :it)))


(defun load-unicode-data ()
  (loop :for ((cp name gc)) :in (load-data "UnicodeData")
        :repeat 1000
        :collect (list (parse-code-point cp) :name name :gc gc)))




(defparameter *symbol-names*
  (remove-duplicates
   (sort (apply #'append (mapcar #'car (append (load-data "PropertyAliases")
                                               (load-data "PropertyValueAliases"))))
         #'string<
         :key #'string-upcase)
   :test #'string=))


(reduce #'+ (mapcar #'length *symbol-names*))
