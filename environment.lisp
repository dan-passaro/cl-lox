(defpackage :cl-lox/environment
  (:documentation "The environment is the object that remembers variables.")
  (:export :make-environment
	   :get-lox-variable)

  (:use :cl)
  (:import-from :cl-lox/token :lexeme))
(in-package :cl-lox/environment)

(defun make-environment ()
  (make-hash-table :test 'equalp))

(defun get-lox-variable (variable-name environment)
  (gethash (lexeme variable-name) environment))
