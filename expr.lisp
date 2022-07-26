(defpackage :cl-lox/expr
  (:export
   :binary
   :make-binary
   :left
   :operator
   :right
   :grouping
   :make-grouping
   :expression
   :literal
   :make-literal
   :value
   :unary
   :make-unary)
  (:use :cl)
  (:import-from :cl-lox/token :token))
(in-package :cl-lox/expr)

(defclass expr () ())

(defclass binary (expr)
  ((left :initarg :left :reader left)
   (operator :initarg :operator :reader operator)
   (right :initarg :right :reader right)))

(defun make-binary (left operator right)
  (make-instance 'binary :left left :operator operator :right right))

(defmethod print-object ((b binary) s)
  (format s "#.(cl-lox/expr:make-binary ~s ~s ~s)"
	  (left b)
	  (operator b)
	  (right b)))

(defclass grouping (expr)
  ((expression :initarg :expression :reader expression)))

(defun make-grouping (expression)
  (make-instance 'grouping :expression expression))

(defmethod print-object ((g grouping) s)
  (format s "#.(cl-lox/expr:make-grouping ~s)"
	  (expression g)))

(defclass literal (expr)
  ((value :initarg :value :reader value)))

(defun make-literal (value)
  (make-instance 'literal :value value))

(defmethod print-object ((l literal) s)
  (format s "#.(cl-lox/expr:make-literal ~s)"
	  (value l)))

(defclass unary (expr)
  ((operator :initarg :operator :reader operator :type token)
   (right :initarg :right :reader right :type expr)))

(defun make-unary (operator right)
  (make-instance 'unary :operator operator :right right))

(defmethod print-object ((u unary) s)
  (format s "#.(cl-lox/expr:make-unary ~s ~s)"
	  (operator u)
	  (right u)))
