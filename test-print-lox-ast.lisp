(defpackage :cl-lox/test-print-lox-ast
  (:use :cl :fiveam :cl-lox/expr :cl-lox/print-lox-ast)
  (:import-from :cl-lox/test-suite :cl-lox-tests)
  (:import-from :cl-lox/equals :equals)
  (:import-from :cl-lox/token :make-token)
  (:import-from :cl-lox/tokens)
  (:shadowing-import-from :cl-lox/expr
   :variable))
(in-package :cl-lox/test-print-lox-ast)

(in-suite cl-lox-tests)

(test prints-parse-trees
  (is (equals "(* (- 123) (group 45.67))"
	      (print-lox-ast
	       (make-binary
		(make-unary (make-token 'cl-lox/tokens:minus "-" nil 1)
			    (make-literal 123))
		(make-token 'cl-lox/tokens:star "*" nil 1)
		(make-grouping (make-literal 45.67)))))))
