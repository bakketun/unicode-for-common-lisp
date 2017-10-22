(defpackage #:unicode-user
  (:use :common-lisp :unicode))

(in-package #:unicode-user)

#.(enable-unicode-syntax)

#16U+FFFF
(utf-16 (utf-32 #xFFFF))


#U+1f4a9
#8U+1f4a9
(list #8u"hello world…")

(utf-32 "hello world…")

(code-point-at "hello world" 1)

(code-point-count "hello")

(utf-16 "hello💩")

(code-point-count "💩a…💩")

(code-point-count (utf-16 "💩a…💩"))

(unicode-string (utf-16 "💩a…💩"))

(code-point-at "h" 0)

(utf-8 "💩a…💩")

(unicode-string (utf-8 (utf-32 (utf-16 (utf-8 (utf-16 "blåbærsyltetøy"))))))

#+sbcl
(sb-ext:octets-to-string (unicode::%utf-8-data (utf-8 (utf-32 (utf-16 (utf-8 (utf-16 "blåbærsyltetøy")))))))

#8U+00A2
#8U+20AC
#8U+10348

(utf-32 #8u+0430)
(utf-32 #8u+4E8C)
(utf-32 #8u+10302)

(defun print-hello ()
  (print (unicode-string #8u"hello world…")))

(print-hello)

(code-point-count (utf-8 #xff #x41))
(code-point-count (utf-8 #xff #x41) :errors :replace)
(code-point-count (utf-8 #xff #x41) :errors :ignore)

(let ((unicode::*transform-errors-default* :ignore))
  (utf-16 (utf-8 #xff #x41)))

(utf-16 :ignore (utf-8 #xff #x41))

(code-point-count (utf-8 #xff #x41) :errors :replace)

(utf-16 :replace (utf-8 #xff #x41) (utf-8 #xff #x41))

(unicode "string" #U+0042 "string" #U+16222)

(utf-8 (utf-8 "hello"))

(list #16u({pile of poo} {greek_capital_letter_delta}))

#u{GREEK_CAPITAL_LETTER_DELTA}


(code-point-at (utf-8 #u{PILE OF POO} "a" #u{PILE OF POO} "a") 3 :errors :ignore)
(code-point-before (utf-8 #u{PILE OF POO} "a" #u{PILE OF POO} "a") 4)
;;(code-point-at (utf-8 "aaa" #xf0 #x90 "bbb") 3)
(code-point-before (utf-8 "aaa" #xf0 #x90 "bbb") 5 :errors :ignore)

(code-point-at (utf-16 #u{PILE OF POO} "a" #u{PILE OF POO} "a") 3)
(code-point-before (utf-16 #u{PILE OF POO} "a" #u{PILE OF POO} "a") 5)
(u16ref (utf-16 #u{PILE OF POO} "a" #u{PILE OF POO} "a") 3)


(utf-8 #\PILE_OF_POO)
(unicode #\PILE_OF_POO)
(unicode-string #\PILE_OF_POO)

(let ((*default-unicode-type* 'utf-8))
  (unicode #U+00ff))

(unicode :replace (utf-8 #xff))
(utf-8 :ignore (utf-8 #xff))

(utf-8 (utf-8 #xff) (utf-8 #xff))

(unicode-string (utf-8 #xF0 #x9F) (utf-8 #x92 #xA9))



(unicode :replace (utf-8 #xF0 #x9F) "a" (utf-8 #x92 #xA9))
(unicode (utf-8 #xF0 #x9F) (utf-8 #x92 #xA9))


(copy-unicode "hello" :type (utf-16))

(concatenate-unicode (list (utf-8 #xF0 #x9F) (utf-8 #x92 #xA9)))
(concatenate-unicode (list (utf-8 #xF0 #x9F) (utf-8 #x92 #xA9)) :type 'utf-16)
(concatenate-unicode (list "A " (utf-8 #xF0 #x9F) (utf-8 #x92 #xA9)))
