(defpackage :cl-lox/report-error
  (:documentation "Error reporting functionality.")
  (:export
   :lox-error
   :report-error)

  (:use :cl :cl-lox/token))
(in-package :cl-lox/report-error)

(defgeneric lox-error (place message))

(defmethod lox-error ((line integer) message)
  (report-error line "" message))

(defmethod lox-error ((token token) message)
  (report-error (line token)
		(if (eq (token-type token) 'cl-lox/tokens:eof)
		    " at end"
		    (format nil " at '~a'" (lexeme token)))
		message))

(defun report-error (line where message)
  (format *error-output* "[line ~a] Error~a: ~a~%" line where message)
  (unless t (warn "set hadError here")))
