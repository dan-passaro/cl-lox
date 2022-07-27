(defpackage :cl-lox/test-parse
  (:use :cl :fiveam :cl-lox/expr)
  (:import-from :cl-lox/test-suite :cl-lox-tests)
  (:import-from :cl-lox/equals :equals)
  (:import-from :cl-lox/parse :parse)
  (:import-from :cl-lox/token :make-token)
  (:import-from :cl-lox/tokens)
  (:import-from :cl-lox/scan :scan-tokens))
(in-package :cl-lox/test-parse)

(in-suite cl-lox-tests)

(defun parse-str (lox-code-str)
  (parse (scan-tokens lox-code-str)))

(defun mk-op (token-type)
  "Make a token used as the operator for some exprs"
  (make-token token-type (symbol-value token-type) nil 1))

(test parse-equality
  (is (equals (make-binary (make-literal 1.0)
			   (mk-op 'cl-lox/tokens:equal-equal)
			   (make-literal 2.0))
	      (parse-str "1 == 2"))))

(test parse-comparison
  (is (equals (make-binary (make-literal 1.0)
			   (mk-op 'cl-lox/tokens:greater)
			   (make-literal 2.0))
	      (parse-str "1 > 2"))))

(test comparison-binds-tighter-than-equality
  (is (equals (make-binary (make-binary (make-literal 1.0)
					(mk-op 'cl-lox/tokens:greater)
					(make-literal 2.0))
			   (mk-op 'cl-lox/tokens:equal-equal)
			   (make-binary (make-literal 3.0)
					(mk-op 'cl-lox/tokens:greater)
					(make-literal 4.0)))
	      (parse-str "1 > 2 == 3 > 4"))))
