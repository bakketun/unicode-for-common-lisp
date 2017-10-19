(defpackage #:unicode-user
  (:use :common-lisp :unicode))

(in-package #:unicode-user)


#16U+FFFF
(utf-16 (utf-32 #xFFFF))


#U+1f4a9
#8U+1f4a9
(list #8u"hello world…")

(utf-32 "hello world…")

(code-point "hello world" 1)

(code-point-count "hello")

(utf-16 "hello💩")

(code-point-count "💩a…💩")

(code-point-count (utf-16 "💩a…💩"))

(unicode-to-string (utf-16 "💩a…💩"))

(code-point "h" 0)

(utf-8 "💩a…💩")

(unicode-to-string (utf-8 (utf-32 (utf-16 (utf-8 (utf-16 "blåbærsyltetøy"))))))

#+sbcl
(sb-ext:octets-to-string (unicode::%utf-8-data (utf-8 (utf-32 (utf-16 (utf-8 (utf-16 "blåbærsyltetøy")))))))

#8U+A2
#8U+20AC
#8U+10348

(utf-32 #8u+0430)
(utf-32 #8u+4E8C)
(utf-32 #8u+10302)

(defun print-hello ()
  (print (unicode-to-string #8u"hello world…")))

(code-point-count (utf-8 #xff #x41))
(code-point-count (utf-8 #xff #x41) :errors :replace)
(code-point-count (utf-8 #xff #x41) :errors :ignore)

(let ((unicode::*transform-errors-default* :ignore))
  (utf-16 (utf-8 #xff #x41)))

(utf-16 :ignore (utf-8 #xff #x41))

(code-point-count (utf-8 #xff #x41) :errors :replace)

(utf-16 :replace (utf-8 #xff #x41) :ignore (utf-8 #xff #x41))
