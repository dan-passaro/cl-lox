(defpackage :cl-lox/evaluate
  (:export :evaluate)
  (:use :cl :cl-lox/equals :cl-lox/expr :cl-lox/token)
  (:shadowing-import-from :cl-lox/expr :literal)
  (:import-from :cl-lox/tokens))
(in-package :cl-lox/evaluate)

(defun is-truthy? (value)
  (not (or (null value)
	   (eq value :false))))

(defgeneric evaluate (expr)
  (:documentation "Evaluate the given Lox expression.

expr is an AST node (i.e. an instance of an expr subtype)"))

(defmethod evaluate ((l literal))
  (literal-value l))

(defmethod evaluate ((g grouping))
  (evaluate (grouping-expression g)))

(defmethod evaluate ((u unary))
  (let ((right (evaluate (unary-right u))))
    (ecase (token-type (unary-operator u))
      (cl-lox/tokens:minus
       (- right))
      (cl-lox/tokens:bang
       (not (is-truthy? right))))))

(defun to-bool (val)
  (or val :false))

(defun lox-is-equal (a b)
  (eql a b))

(defmethod evaluate ((b binary))
  (let ((left (evaluate (binary-left b)))
	(right (evaluate (binary-right b))))
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
