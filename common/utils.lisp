(in-package #:unicode-common)


(defmacro with-gensyms (symbols &body body)
  `(let ,(loop for symbol in symbols collect `(,symbol ',(make-symbol (string symbol))))
    ,@body))
