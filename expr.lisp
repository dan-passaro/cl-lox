(defpackage :cl-lox/expr
  (:export
   :binary
   :make-binary
   :binary-left
   :binary-operator
   :binary-right
   :grouping
   :make-grouping
   :grouping-expression
   :literal
   :make-literal
   :literal-value
   :unary
   :make-unary
   :unary-operator
   :unary-right)
  (:use :cl)
  (:import-from :cl-lox/token :token))
(in-package :cl-lox/expr)

(defstruct expr)

(defstruct (binary (:include expr)
		   (:constructor make-binary (left operator right)))
  left
  operator
  right)

(defmethod print-object ((b binary) s)
  (format s "#.(cl-lox/expr:make-binary ~s ~s ~s)"
	  (binary-left b)
	  (binary-operator b)
	  (binary-right b)))

(defstruct (grouping (:include expr)
		     (:constructor make-grouping (expression)))
  expression)

(defmethod print-object ((g grouping) s)
  (format s "#.(cl-lox/expr:make-grouping ~s)"
	  (grouping-expression g)))

(defstruct (literal (:include expr)
		    (:constructor make-literal (value)))
  value)

(defmethod print-object ((l literal) s)
  (format s "#.(cl-lox/expr:make-literal ~s)"
	  (literal-value l)))

(defstruct (unary (:include expr)
		  (:constructor make-unary (operator right)))
  operator
  right)

(defmethod print-object ((u unary) s)
  (format s "#.(cl-lox/expr:make-unary ~s ~s)"
	  (unary-operator u)
	  (unary-right u)))
