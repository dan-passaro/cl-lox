(defpackage :cl-lox/parse
  (:export :parse)
  (:use :cl :cl-lox/equals :cl-lox/expr :cl-lox/stmt :cl-lox/token)
  (:shadowing-import-from :cl-lox/expr
   :literal
			  :variable)
  (:shadowing-import-from :cl-lox/tokens
   :print
			  :equal)
  (:import-from :cl-lox/lox-parse-error :lox-parse-error)
  (:import-from :cl-lox/report-error :lox-error)
  (:import-from :cl-lox/tokens
   :semicolon
		:greater
   :greater-equal
		:less
   :less-equal
		:bang-equal
   :equal-equal
		:identifier))
(in-package :cl-lox/parse)

(defun lox-parse-error (token message)
  (lox-error token message)
  (make-condition 'lox-parse-error))

(defun parse (tokens)
  (setf tokens (make-array (length tokens) :adjustable t
					   :initial-contents tokens))
  (let ((current 0))
    (labels ((expression ()
	       (equality))

	     (declaration ()
	       (handler-case
		   (if (match 'cl-lox/tokens:var)
		       (var-declaration)
		       (statement))
		 (lox-parse-error ()
		   (synchronize)
		   nil)))

	     (var-declaration ()
	       (let ((name (consume 'identifier
				    "Expect variable name."))
		     initializer)
		 (when (match 'equal)
		   (setf initializer (expression)))
		 (consume 'semicolon
			  "Expect ';' after variable declaration.")
		 (make-var-stmt name initializer)))

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
	       (cond ((match 'cl-lox/tokens:false)
		      (make-literal :false))
		     ((match 'cl-lox/tokens:true)
		      (make-literal t))
		     ((match 'cl-lox/tokens:nil)
		      (make-literal nil))
		     ((match 'cl-lox/tokens:number
			'cl-lox/tokens:string)
		      (make-literal
		       (cl-lox/token:literal (previous))))
		     ((match 'identifier)
		      (make-variable (previous)))
		     ((match 'cl-lox/tokens:left-paren)
		      (let ((expr (expression)))
			(consume 'cl-lox/tokens:right-paren
			    "Expect ')' after expression.")
			(make-grouping expr)))
		     (t (error (lox-parse-error (peek)
						"Expect expression.")))))

	     (match (&rest token-types)
	       (when (some (function check) token-types)
		 (advance)
		 t))

	     (check (tok-type)
	       (and (not (at-end?)) (equals (token-type (peek)) tok-type)))

	     (advance ()
	       (unless (at-end?) (incf current))
	       (previous))

	     (consume (tok-type error-message)
	       (if (check tok-type) (advance)
		   (error (lox-parse-error (peek) error-message))))

	     (at-end? ()
	       (eq (token-type (peek)) 'cl-lox/tokens:eof))

	     (peek () (aref tokens current))

	     (previous () (aref tokens (1- current)))

	     (synchronize ()
	       (advance)
	       (loop while (not (at-end?))
		     do
			(when (eq (token-type (previous)) 'semicolon)
			  (return-from synchronize))
			(when (member (token-type (peek))
				      '(cl-lox/tokens:class
					cl-lox/tokens:fun
					cl-lox/tokens:var
					cl-lox/tokens:for
					cl-lox/tokens:if
					cl-lox/tokens:while
					cl-lox/tokens:print
					cl-lox/tokens:return))
			  (return-from synchronize))
			(advance))))
      (loop until (at-end?) collect (declaration)))))
