(in-package #:unicode-base)

#-(or string-is-utf-8
      string-is-utf-16
      string-is-utf-32)
(cond ((<= +code-point-limit+ char-code-limit)
       (pushnew :string-is-utf-32 *features*))
      ((= +bmp-code-point-limit+ char-code-limit)
       (pushnew :string-is-utf-16 *features*))
      ((<= #xFF char-code-limit)
       (pushnew :string-is-utf-8 *features*)))
