(defpackage :cl-lox/test-evaluate
  (:use :cl :fiveam)
  (:import-from :cl-lox/test-suite :cl-lox-tests)
  (:import-from :cl-lox/equals :equals)
  (:import-from :cl-lox/evaluate :evaluate)
  (:import-from :cl-lox/parse :parse)
  (:import-from :cl-lox/scan :scan-tokens))
(in-package :cl-lox/test-evaluate)

(in-suite cl-lox-tests)

(defun eval-lox-expr (lox-expr-string)
  (evaluate (parse (scan-tokens lox-expr-string))))

(test evaluates-literals
  (is (equals 3.0 (eval-lox-expr "3.0")))
  (is (equals "hello world" (eval-lox-expr "\"hello world\""))))

(test evaluates-groupings
  (is (equals 3.0 (eval-lox-expr "(3.0)")))
  (is (equals "hello world" (eval-lox-expr "(\"hello world\")"))))
