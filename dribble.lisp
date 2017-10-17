(defpackage #:unicode-user
  (:use :common-lisp :unicode))

(in-package #:unicode-user)


#U+1f4a9
#8U+1f4a9
(list #8u"hello world…")

(utf-32 "hello world…")

(code-point "hello world" 1)

(code-point-count "hello")

(utf-16 "hello💩")

(code-point-count (utf-16 "💩a…💩"))

(unicode-to-string (utf-16 "💩a…💩"))

(code-point "h" 0)

(utf-8 "💩a…💩")

(sb-ext:octets-to-string (unicode::%utf-8-data (utf-8 (utf-32 (utf-16 (utf-8 (utf-16 "blåbærsyltetøy")))))))

#8U+A2
#8U+20AC
#8U+10348

(defun print-hello ()
  (print (unicode-to-string #8u"hello world…")))
