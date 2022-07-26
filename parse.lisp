(defpackage :cl-lox/parse
  (:export :parse)
  (:use :cl :cl-lox/equals :cl-lox/expr :cl-lox/token)
  (:shadowing-import-from :cl-lox/expr :literal)
  (:import-from :cl-lox/tokens))
(in-package :cl-lox/parse)

(defun parse (tokens)
  (setf tokens (make-array (length tokens) :adjustable t
					   :initial-contents tokens))
  (let ((current 0))
    (labels ((expression ()
	       (equality))

	     (equality ()
	       (let ((expr (primary)))
		 (loop while (match 'cl-lox/tokens:bang-equal
			       'cl-lox/tokens:equal-equal)
		       do (let ((operator (previous))
				(right (primary)))
			    (setf expr (make-binary expr operator right))))
		 expr))

	     (primary ()
	       (when (match 'cl-lox/tokens:false)
		 (return-from primary (make-literal :false)))
	       (when (match 'cl-lox/tokens:true)
		 (return-from primary (make-literal t)))
	       (when (match 'cl-lox/tokens:nil)
		 (return-from primary (make-literal nil)))
	       (when (match 'cl-lox/tokens:number 'cl-lox/tokens:string)
		 (return-from primary (make-literal
				       (cl-lox/token:literal (previous)))))
	       (when (match 'cl-lox/tokens:right-paren)
		 (let ((expr (expression)))
		   (consume 'cl-lox/tokens:right-paren
			    "Expect ')' after expression.")
		   (make-grouping expr))))

	     (match (&rest token-types)
	       (and (some (function check) token-types)
		    (progn (advance) t)))

	     (check (tok-type)
	       (and (not (at-end?)) (equals (token-type (peek)) tok-type)))

	     (advance ()
	       (when (not (at-end?))
		 (incf current))
	       (previous))

	     (consume (tok-type error-message)
	       (if (check tok-type) (advance)
		   (error (format nil "~s ~s" (peek) error-message))))

	     (at-end? ()
	       (eq (token-type (peek)) 'cl-lox/tokens:eof))

	     (peek () (aref tokens current))

	     (previous () (aref tokens (1- current))))
      (expression))))
