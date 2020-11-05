(defmacro with-gensyms (syms &body body)
  `(let ,(loop for s in syms collect `(,s (gensym ,(symbol-name s))))
    ,@body))

(defmacro define-decoder (name (&rest state) &body body)
  (let ((variables (mapcar #'car state))
        (handler-name (intern (format nil "~A-HANDLER" name) (symbol-package name))))
    `(progn
       ,(with-gensyms (status /code-point/ unread-byte)
          `(defun ,handler-name (%eofp %byte ,@variables &aux ,unread-byte)
             (flet ((%return (,status &optional (,/code-point/ 0))
                      (return-from ,handler-name (values ,/code-point/
                                                         ,status
                                                         ,unread-byte
                                                         ,@variables)))
                    (%unread-byte ()
                      (setf ,unread-byte %byte)))
               ,@body)))
       ,(with-gensyms (bytes returned-code-point status unread-byte decode-1 eofp byte i)
          `(defun ,name (,bytes)
             (let ((,returned-code-point)
                   (,status)
                   (,unread-byte)
                   ,@state)
               (flet ((,decode-1 (,eofp ,byte)
                        (multiple-value-setq
                            (,returned-code-point ,status ,unread-byte ,@variables)
                          (,handler-name ,eofp ,byte ,@variables)))
                      (,returned-code-point (,byte)
                        (case ,status
                          (%code-point ,returned-code-point)
                          (otherwise
                           (list ,byte ,status ,unread-byte
                                 (list ,@variables))))))
                 (loop :for ,byte :across ,bytes
                       :for ,i :downfrom (1- (length ,bytes))
                       :do (,decode-1 nil ,byte)
                       :collect (,returned-code-point ,byte)
                       :when ,unread-byte
                         :do (,decode-1 nil ,unread-byte)
                         :and :collect (,returned-code-point ,unread-byte)
                       :when (zerop ,i)
                         :do (,decode-1 t 0)
                         :and :collect (,returned-code-point :eof)))))))))


;; https://encoding.spec.whatwg.org/#utf-8-decoder

(define-decoder utf-8-decoder ((code-point      0)
                               (bytes-seen      0)
                               (bytes-needed    0)
                               (lower-boundary  #x80)
                               (upper-boundary  #xbf))
  ;; 1
  (when (and %eofp (plusp bytes-needed))
    (setf bytes-needed 0)
    (%return :truncated-sequence))
  ;; 2
  (when %eofp
    (%return '%finished))
  ;; 3
  (when (zerop bytes-needed)
    (typecase %byte
      ;; 0x00 to 0x7F
      ((integer #x00 #x7f)  (%return '%code-point %byte))
      ;; 0xC2 to 0xDF
      ((integer #xc2 #xdf)  (setf bytes-needed    1
                                  code-point      (ldb (byte 5 0) %byte)))
      ;; 0xE0 to 0xEF
      ((integer #xe0 #xef) (progn
                             (when (eql %byte #xe0)  (setf lower-boundary #xa0))
                             (when (eql %byte #xed)  (setf upper-boundary #x9f))
                             (setf bytes-needed  2
                                   code-point    (ldb (byte 4 0) %byte))))
      ;; 0xF0 to 0xF4
      ((integer #xF0 #xF4) (progn
                             (when (eql %byte #xf0)  (setf lower-boundary #x90))
                             (when (eql %byte #xf4)  (setf upper-boundary #x8f))
                             (setf bytes-needed  3
                                   code-point    (ldb (byte 3 0) %byte))))
      ;; Otherwise
      (t                    (%return :invalid-first-byte)))
    (%return '%continue))
  ;; 4
  (unless (<= lower-boundary %byte upper-boundary)
    (setf code-point      0
          bytes-needed    0
          bytes-seen      0
          lower-boundary  #x80
          upper-boundary  #xBF)
    (%unread-byte)
    (%return :invalid-second-byte))
  ;; 5
  (setf lower-boundary  #x80
        upper-boundary  #xBF)
  ;; 6
  (setf code-point (logior (ash code-point 6) (ldb (byte 6 0) %byte)))
  ;; 7
  (incf bytes-seen)
  ;; 8
  (unless (eql bytes-seen bytes-needed)
    (%return '%continue))
  ;; 9
  (let ((/code-point/ code-point))
    ;; 10
    (setf code-point    0
          bytes-needed  0
          bytes-seen    0)
    ;; 11
    (%return '%code-point /code-point/)))



(utf-8-decoder #(65 66 67))
(utf-8-decoder #(65 66 67 #xe2 65))
(utf-8-decoder #(65 66 67 #xe2))
