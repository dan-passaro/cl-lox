(defpackage :cl-lox/environment
  (:documentation "The environment is the object that remembers variables.")
  (:export :make-environment
	   :get-lox-variable)

  (:use :cl)
  (:import-from :cl-lox/lox-runtime-error :lox-runtime-error)
  (:import-from :cl-lox/token :lexeme))
(in-package :cl-lox/environment)

(defun make-environment ()
  (make-hash-table :test 'equalp))

(defun get-lox-variable (variable-name environment)
  (multiple-value-bind
	(value key-found?)
      (gethash (lexeme variable-name) environment)
    (unless key-found?
      (error 'lox-runtime-error
	     :token variable-name
	     :message (format nil "Undefined variable '~a'."
			      (lexeme variable-name))))
    value))
