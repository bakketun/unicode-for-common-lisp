(in-package #:unicode-common)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun features-detect (cclimit)
    (pushnew (cond ((<= +code-point-limit+ cclimit)
                    :string-is-utf-32)
                   ((= +bmp-code-point-limit+ cclimit)
                    :string-is-utf-16)
                   ((<= #xFF cclimit)
                    :string-is-utf-8)
                   (t
                    :string-is-not-unicode))
             *features*))

  (defun enabled-features ()
    (intersection '(:string-is-utf-8
                    :string-is-utf-16
                    :string-is-utf-32
                    :string-is-not-unicode)
                  *features*))

  (unless (enabled-features)
    (features-detect char-code-limit))

  (assert (= 1 (length (enabled-features))) ()
          "More than one Unicode feature in *features*: ~S." (enabled-features)))


(let ((at-read-time #.(car (enabled-features)))
      (at-run-time (car (enabled-features))))
  (assert (eql at-read-time at-run-time) ()
          "Unicode feature changed from ~S to ~S. Please do clean recompile." at-read-time at-run-time))
