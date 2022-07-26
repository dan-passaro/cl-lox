(defpackage :cl-lox/token
  (:use :cl :cl-lox/equals)
  (:export
   :make-token
   :token
   :token-type
   :lexeme
   :literal
   :line))
(in-package :cl-lox/token)

(defclass token ()
  ((token-type :reader token-type :initarg :token-type)
   (lexeme :reader lexeme :initarg :lexeme)
   (literal :reader literal :initarg :literal)
   (line :reader line :initarg :line)))

(defun make-token (token-type lexeme literal line)

  ;; TODO: this should be a declaim or something so this is a compile
  ;; time only check. But I don't know how to do that in CL yet :)
  (check-type token-type symbol)

  (make-instance 'token :token-type token-type
			:lexeme lexeme
			:literal literal
			:line line))

(defmethod print-object ((token token) stream)
  (format stream "#.(cl-lox:make-token '~s ~s ~s ~s)"
	  (token-type token)
	  (lexeme token)
	  (literal token)
	  (line token)))

(defmethod equals ((a token) (b token))
  (and (equals (token-type a) (token-type b))
       (equals (lexeme a) (lexeme b))
       (equals (literal a) (literal b))
       (equals (line a) (line b))))
