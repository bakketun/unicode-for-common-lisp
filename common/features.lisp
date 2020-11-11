(in-package #:unicode-common)


#-(or string-is-utf-8        ;; char-code = UTF-8 code unit
      string-is-utf-16       ;; char-code = UTF-16 code unit
      string-is-utf-32       ;; char-code = UTF-32 code unit
      string-is-not-unicode  ;; char-code = something else, e.g. EBCDIC
      )
(pushnew (cond ((<= +code-point-limit+ char-code-limit)
                :string-is-utf-32)
               ((= +bmp-code-point-limit+ char-code-limit)
                :string-is-utf-16)
               ((<= #xFF char-code-limit)
                :string-is-utf-8)
               (t
                :string-is-not-unicode))
         *features*)

(let ((enabled-features (intersection '(:string-is-utf-8
                                        :string-is-utf-16
                                        :string-is-utf-32
                                        :string-is-not-unicode)
                                      *features*)))
  (assert (= 1 (length enabled-features)) ()
          "More than one Unicode feature in *features* ~S.~&Try a clean recompile." enabled-features))
