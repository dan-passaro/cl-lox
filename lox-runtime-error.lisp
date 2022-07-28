(defpackage :cl-lox/lox-runtime-error
  (:export :lox-runtime-error
   :message
   :token)
  (:use :cl))
(in-package :cl-lox/lox-runtime-error)

(define-condition lox-runtime-error (error)
  ((token :initarg :token :reader token)
   (message :initarg :message :reader message)))
