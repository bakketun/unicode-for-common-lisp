(in-package #:unicode-base)


(defgeneric map-scalar-values (function scalar-value-sequence)
  (:documentation "Results from function ignored, for now."))


(defgeneric scalar-value-vector (scalar-value-sequence)
  (:documentation "Return all scalar values as vector of scalar-value elements."))
