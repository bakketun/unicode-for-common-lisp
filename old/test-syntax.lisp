(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable))
  (unicode:enable-unicode-text-syntax))

(list
  ;; Hex code point
  #U+1F313
  #8U+1F313
  #16U+1F313
  #32U+1F313
  #0U+1F313
  ;;#U+1F313x
  #U+1F313;;test

  ;; Named code point
  #U{PILE OF POO}
  #U{GREEK CAPITAL LETTER DELTA}

  ;; String syntax
  #U"Hello World"
  #U "Hello World"

  (list :string-syntax
        #0U"string syntax"
        #8U"UTF-8 string syntax"
        #16U"UTF-16 string syntax"
        #32U"UTF-32 string syntax")

  ;; Code units
  #8U<F0 9F 92 A9>
  #8U<F0 9F>
  #16U<FFFE FFFF>
  #32U<41 1F314 42>

  (list
    ;; List syntax
    #8U("Hello"
        U+1F313 ;comment
        U+1F313<>
        U+1F313{PILE OF POO}
        U+1F313 U+1F313
        "World "
        {PILE OF POO} ; comment
        <F0 ; hello >
        9F #| hello |# 92 A9>
        <F0>
        )))
