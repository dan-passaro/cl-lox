(defpackage :cl-lox/stmt
  (:documentation "Statement types for the Lox interpreter.")
  (:export
   :print-stmt
   :make-print-stmt
   :print-stmt-expression
   :expression-stmt
   :make-expression-stmt
   :expression-stmt-expression
   :stmt)
  (:use :cl))
(in-package :cl-lox/stmt)

(defstruct stmt)

(defstruct (expression-stmt (:include stmt)
			    (:constructor make-expression-stmt (expression)))
  expression)

(defmethod print-object ((e expression-stmt) s)
  (format s "#.(cl-lox/stmt:make-expression-stmt ~s)"
	  (expression-stmt-expression e)))

(defstruct (print-stmt (:include stmt)
		       (:constructor make-print-stmt (expression)))
  expression)

(defmethod print-object ((p print-stmt) s)
  (format s "#.(cl-lox/stmt:make-print-stmt ~s)"
	  (print-stmt-expression p)))
