(defpackage :cl-lox/interpret
  (:export :interpret)

  (:use :cl :cl-lox/stmt)
  (:import-from :str)
  (:import-from :cl-lox/evaluate :evaluate))
(in-package :cl-lox/interpret)

(defun stringify (lox-value)
  (cond ((null lox-value) "nil")
	((eq lox-value t) "true")
	((eq lox-value :false) "false")
	((floatp lox-value)
	 (let ((text (princ-to-string lox-value)))
	   (if (str:ends-with? ".0" text)
	       (subseq text 0 (- (length text) 2))
	       text)))
	(t (princ-to-string lox-value))))

(defgeneric interpret (statement)
  (:documentation "Interpret the given Lox statement.

Alternatively, if given a list, interprets all statements in that list"))

(defmethod interpret ((statements list))
  (dolist (statement statements) (interpret statement)))

(defmethod interpret ((p print-stmt))
  (let ((value (evaluate (print-stmt-expression p))))
    (format t "~a~%" (stringify value))))

(defmethod interpret ((v var-stmt)))
