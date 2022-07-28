(defpackage :cl-lox/interpret
  (:export :interpret
	   :had-runtime-error)

  (:use :cl :cl-lox/stmt)
  (:import-from :str)
  (:import-from :cl-lox/environment :make-environment)
  (:import-from :cl-lox/evaluate :evaluate)
  (:import-from :cl-lox/lox-runtime-error
   :lox-runtime-error
		:message
   :token)
  (:import-from :cl-lox/token
   :lexeme
		:line))
(in-package :cl-lox/interpret)

(defparameter *had-runtime-error* nil)

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

(defun report-lox-runtime-error (err)
  (format *error-output* "~a~%[line ~a]~%"
	  (message err)
	  (line (token err)))
  (setf *had-runtime-error* t))

(defgeneric interpret (statement environment)
  (:documentation "Interpret the given Lox statement.

Alternatively, if given a list, interprets all statements in that list"))

(defmethod interpret ((statements list) environment)
  (unless environment (setf environment (make-environment)))
  (handler-case
      (dolist (statement statements) (interpret statement environment))
    (lox-runtime-error (err) (report-lox-runtime-error err))))

(defmethod interpret ((p print-stmt) environment)
  (let ((value (evaluate (print-stmt-expression p) environment)))
    (format t "~a~%" (stringify value))))

(defmethod interpret ((v var-stmt) environment)

  ;; TODO: handle statements with no initializers
  (let ((value (evaluate (var-stmt-initializer v) environment)))
    (setf (gethash (lexeme (var-stmt-name v)) environment)
	  value)))
