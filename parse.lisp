(defpackage :cl-lox/parse
  (:export :parse)
  (:use :cl :cl-lox/equals :cl-lox/expr :cl-lox/stmt :cl-lox/token)
  (:shadowing-import-from :cl-lox/expr :literal)
  (:shadowing-import-from :cl-lox/tokens :print)
  (:import-from :cl-lox/tokens
   :semicolon
		:greater
   :greater-equal
		:less
   :less-equal
		:bang-equal
		:equal-equal))
(in-package :cl-lox/parse)

(defun parse (tokens)
  (setf tokens (make-array (length tokens) :adjustable t
					   :initial-contents tokens))
  (let ((current 0))
    (labels ((expression ()
	       (equality))

	     (statement ()
	       (if (match 'print) (print-statement)
		   (expression-statement)))

	     (print-statement ()
	       (let ((value (expression)))
		 (consume 'semicolon "Expect ';' after value.")
		 (make-print-stmt value)))

	     (expression-statement ()
	       (let ((expr (expression)))
		 (consume 'semicolon "Expect ';' after expression")
		 (make-expression-stmt expr)))

	     (equality ()
	       (let ((expr (comparison)))
		 (loop while (match 'bang-equal 'equal-equal)
		       do (let ((operator (previous))
				(right (comparison)))
			    (setf expr (make-binary expr operator right))))
		 expr))

	     (comparison ()
	       (let ((expr (term)))
		 (loop while (match 'greater 'greater-equal 'less 'less-equal)
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
	       (when (match 'cl-lox/tokens:left-paren)
		 (let ((expr (expression)))
		   (consume 'cl-lox/tokens:right-paren
			    "Expect ')' after expression.")
		   (return-from primary (make-grouping expr))))
	       (error (format nil "Expect expression: ~s" (peek))))

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
      (loop until (at-end?) collect (statement)))))
