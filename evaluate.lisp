(defpackage :cl-lox/evaluate
  (:export :evaluate)
  (:use :cl :cl-lox/equals :cl-lox/expr :cl-lox/token)
  (:shadowing-import-from :cl-lox/expr :literal)
  (:import-from :cl-lox/tokens))
(in-package :cl-lox/evaluate)

(defgeneric evaluate (expr)
  (:documentation "Evaluate the given Lox expression.

expr is an AST node (i.e. an instance of an expr subtype)"))

(defmethod evaluate ((l literal))
  (literal-value l))

(defmethod evaluate ((g grouping))
  (evaluate (grouping-expression g)))
