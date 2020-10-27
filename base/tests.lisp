(assert (subtypep 'surrogate-code-point 'code-point))
(assert (subtypep 'scalar-value 'code-point))
(when (eq #\Newline (name-char "Linefeed"))
  (assert (subtypep 'standard-char 'code-point)))
