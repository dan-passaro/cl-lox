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
	       (let ((expr (comparison)))
		 (loop while (match 'cl-lox/tokens:bang-equal
			       'cl-lox/tokens:equal-equal)
		       do (let ((operator (previous))
				(right (comparison)))
			    (setf expr (make-binary expr operator right))))
		 expr))

	     (comparison ()
	       (let ((expr (term)))
		 (loop while (match
				 'cl-lox/tokens:greater
			       'cl-lox/tokens:greater-equal
			       'cl-lox/tokens:less
			       'cl-lox/tokens:less-equal)
		       do (let ((operator (previous))
				(right (term)))
			    (setf expr (make-binary expr operator right))))
		 expr))

	     (term ()
	       (let ((expr (factor)))
		 (loop while (match 'cl-lox/tokens:minus 'cl-lox/tokens:plus)
		       do (let ((operator (previous))
				(right (factor)))
			    (setf expr (make-binary expr operator right))))
		 expr))

	     (factor ()
	       (let ((expr (unary)))
		 (loop while (match 'cl-lox/tokens:slash 'cl-lox/tokens:star)
		       do (let ((operator (previous))
				(right (unary)))
			    (setf expr (make-binary expr operator right))))
		 expr))

	     (unary ()
	       (if (match 'cl-lox/tokens:bang 'cl-lox/tokens:minus)
		   (let ((operator (previous))
			 (right (unary)))
		     (make-unary operator right))
		   (primary)))

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
		   (return-from primary (make-grouping expr))))
	       (error (format nil "unexpected token ~s" (peek))))

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
