(assert (subtypep 'surrogate-code-point 'code-point))
(assert (subtypep 'scalar-value 'code-point-code))
(assert (not (code-point #\Newline)))
