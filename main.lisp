(defpackage :cl-lox/main
  (:nicknames :cl-lox)
  (:use :cl)
  (:export :run))
(in-package :cl-lox/main)

(defun run (code-str)
  "Parse and then execute code-str."
  (declare (ignore code-str))
  (format t "Hello~%"))
