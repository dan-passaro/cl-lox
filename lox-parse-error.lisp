(defpackage :cl-lox/lox-parse-error
  (:export :lox-parse-error)
  (:use :cl))
(in-package :cl-lox/lox-parse-error)

(define-condition lox-parse-error (error) ())
