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

(test evaluate-literals
  (is (equals 3.0 (eval-lox-expr "3.0")))
  (is (equals "hello world" (eval-lox-expr "\"hello world\""))))

(test evaluate-groupings
  (is (equals 3.0 (eval-lox-expr "(3.0)")))
  (is (equals "hello world" (eval-lox-expr "(\"hello world\")"))))

(test evaluate-unary
  (is (equals -15.5 (eval-lox-expr "-15.5")))
  (is (equals t (eval-lox-expr "!false"))))

(test evaluate-binary-operators
  (is (equals 2 (eval-lox-expr "6 - 4")))
  (is (equals 12 (eval-lox-expr "6 * 2")))
  (is (equals 4 (eval-lox-expr "40 / 10")))
  (is (equals 10 (eval-lox-expr "5 + 5")))
  (is (equals "foobar" (eval-lox-expr "\"foo\" + \"bar\""))))
