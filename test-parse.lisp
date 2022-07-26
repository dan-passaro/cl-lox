(defpackage :cl-lox/test-parse
  (:use :cl :fiveam)
  (:import-from :cl-lox/test-suite :cl-lox-tests)
  (:import-from :cl-lox/equals :equals))
(in-package :cl-lox/test-parse)

(in-suite cl-lox-tests)

(test scanner-test
  (is (string= "foo" "foo")))
