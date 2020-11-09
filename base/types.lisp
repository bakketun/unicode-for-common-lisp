(in-package #:unicode-base)

(deftype code-point ()
  `(integer ,+first-code-point+ ,+last-code-point+))

(deftype bmp-code-point ()
  `(integer ,+first-code-point+ ,+last-bmp-code-point+))

(deftype high-surrogate ()
  `(integer ,+first-high-surrogate+ ,+last-high-surrogate+))

(deftype low-surrogate ()
  `(integer ,+first-low-surrogate+ ,+last-low-surrogate+))

(deftype surrogate ()
  '(or high-surrogate low-surrogate))

(deftype scalar-value ()
  '(and
    code-point
    (not surrogate)))
