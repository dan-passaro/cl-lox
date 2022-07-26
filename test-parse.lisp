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

(test parse-equality
  (is (equals (make-binary (make-literal 1.0)
			   (make-token 'cl-lox/tokens:equal-equal
				       "=="
				       nil
				       1)
			   (make-literal 2.0))
	      (parse-str "1 == 2"))))
