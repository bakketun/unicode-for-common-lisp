;;(pushnew :string-is-utf-8 *features*)
;;(pushnew :string-is-utf-16 *features*)
;;(pushnew :string-is-utf-32 *features*)

#-(or string-is-utf-8
      string-is-utf-16
      string-is-utf-32)
(cond ((<= 1114112 char-code-limit)
       (pushnew :string-is-utf-32 *features*))
      ((= #x10000 char-code-limit)
       (pushnew :string-is-utf-16 *features*))
      ((<= 256 char-code-limit)
       (pushnew :string-is-utf-8 *features*)))
