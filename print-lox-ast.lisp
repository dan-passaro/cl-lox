(defpackage :cl-lox/print-lox-ast
  (:export :print-lox-ast)
  (:use :cl :cl-lox/expr)
  (:import-from :cl-lox/token))
(in-package :cl-lox/print-lox-ast)

(defgeneric print-lox-ast (expr)
  (:documentation "Create a string showing EXPR and all children"))

(defun parenthesize (name &rest exprs)
  (format nil "(~a ~{~a~^ ~})" name (mapcar 'print-lox-ast exprs)))

(defmethod print-lox-ast ((b binary))
  (parenthesize (cl-lox/token:lexeme (binary-operator b))
		(binary-left b)
		(binary-right b)))

(defmethod print-lox-ast ((g grouping))
  (parenthesize "group" (grouping-expression g)))

(defmethod print-lox-ast ((l literal))
  (let ((val (literal-value l)))
    (if (null val)
	"nil"
	(write-to-string val))))

(defmethod print-lox-ast ((u unary))
  (parenthesize (cl-lox/token:lexeme (unary-operator u))
		(unary-right u)))
