(defpackage :cl-lox/evaluate
  (:export :evaluate)
  (:use :cl :cl-lox/equals :cl-lox/expr :cl-lox/token)
  (:shadowing-import-from :cl-lox/expr
   :literal
			  :variable)
  (:import-from :cl-lox/environment :get-lox-variable)
  (:import-from :cl-lox/tokens))
(in-package :cl-lox/evaluate)

(defun is-truthy? (value)
  (not (or (null value)
	   (eq value :false))))

(defgeneric evaluate (expr environment)
  (:documentation "Evaluate the given Lox expression.

expr is an AST node (i.e. an instance of an expr subtype)"))

(defmethod evaluate ((l literal) environment)
  (literal-value l))

(defmethod evaluate ((g grouping) environment)
  (evaluate (grouping-expression g) environment))

(defmethod evaluate ((u unary) environment)
  (let ((right (evaluate (unary-right u) environment)))
    (ecase (token-type (unary-operator u))
      (cl-lox/tokens:minus
       (- right))
      (cl-lox/tokens:bang
       (not (is-truthy? right))))))

(defun to-bool (val)
  (or val :false))

(defun lox-is-equal (a b)
  (eql a b))

(defmethod evaluate ((b binary) environment)
  (let ((left (evaluate (binary-left b) environment))
	(right (evaluate (binary-right b) environment)))
    (ecase (token-type (binary-operator b))
      (cl-lox/tokens:plus
       (if (and (stringp left) (stringp right))
	   (concatenate 'string left right)
	   (+ left right)))
      (cl-lox/tokens:minus (- left right))
      (cl-lox/tokens:slash (/ left right))
      (cl-lox/tokens:star (* left right))
      (cl-lox/tokens:less (to-bool (< left right)))
      (cl-lox/tokens:less-equal (to-bool (<= left right)))
      (cl-lox/tokens:greater (to-bool (> left right)))
      (cl-lox/tokens:greater-equal (to-bool (>= left right)))
      (cl-lox/tokens:equal-equal (to-bool (equals left right))))))

(defmethod evaluate ((v variable) environment)
  (get-lox-variable (variable-name v) environment))
