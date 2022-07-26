(defpackage :cl-lox/test-suite
  (:export :cl-lox-tests)
  (:use :cl :fiveam))
(in-package :cl-lox/test-suite)

(def-suite cl-lox-tests
  :description "All tests for CL-LOX")
