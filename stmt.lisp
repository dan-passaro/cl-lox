(defpackage :cl-lox/stmt
  (:documentation "Statement types for the Lox interpreter.")
  (:export
   :stmt
   :print-stmt
   :make-print-stmt
   :print-stmt-expression
   :expression-stmt
   :make-expression-stmt
   :expression-stmt-expression
   :var-stmt
   :make-var-stmt
   :var-stmt-name
   :var-stmt-initializer)
  (:use :cl)
  (:import-from :cl-lox/expr :expr)
  (:import-from :cl-lox/token :token))
(in-package :cl-lox/stmt)

(defstruct stmt)

(defstruct (expression-stmt (:include stmt)
			    (:constructor make-expression-stmt (expression)))
  expression)

(defmethod print-object ((e expression-stmt) s)
  (format s "#.(~(~s~) ~s)"
	  'make-expression-stmt
	  (expression-stmt-expression e)))

(defstruct (print-stmt (:include stmt)
		       (:constructor make-print-stmt (expression)))
  expression)

(defmethod print-object ((p print-stmt) s)
  (format s "#.(~(~s~) ~s)"
	  'make-print-stmt
	  (print-stmt-expression p)))

(defstruct (var-stmt (:include stmt)
		     (:constructor make-var-stmt
			 (name initializer)))
  (name nil :type token)
  (initializer nil :type expr))

(defmethod print-object ((v var-stmt) s)
  (format s "#.(~(~s~) ~s ~s)"
	  'make-var-stmt
	  (var-stmt-name v)
	  (var-stmt-initializer v)))
