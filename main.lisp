(defpackage :cl-lox/main
  (:nicknames :cl-lox)
  (:use :cl)
  (:export :run))
(in-package :cl-lox/main)

(defun run (code-str)
  "Parse and then execute code-str."

  ;; For now, hardcoded to assume a one-line "print" instruction. MVP :)
  (let* ((first-quote-pos (position #\" code-str))
	 (second-quote-pos (position #\" code-str :start (1+ first-quote-pos)))
	 (str-content (subseq code-str (1+ first-quote-pos) second-quote-pos)))
  (format t "~a~%" str-content)))
