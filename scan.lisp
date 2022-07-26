(defpackage :cl-lox/scan
  (:export :scan-tokens)

  (:use :cl :cl-lox/equals)
  (:import-from :cl-lox/token :make-token)
  (:import-from :cl-lox/tokens))
(in-package :cl-lox/scan)

(defconstant tokens-package (find-package :cl-lox/tokens))

(defun scan-tokens (source)
  (let ((start 0)
	(current 0)
	(line 1)
	(tokens nil))
    (labels ((is-at-end () (>= current (length source)))

	     (advance () (prog1
			     (elt source current)
			   (incf current)))

	     (add-token (type &optional literal)
	       (let ((text (subseq source start current)))
		 (push (make-token type text literal line) tokens)))

	     (match (expected)
	       (cond ((is-at-end) nil)
		     ((string/= (elt source current) expected) nil)
		     (t (incf current) t)))

	     (peek ()
	       (if (is-at-end) #\Nul (elt source current)))

	     (peek-next ()
	       (if (>= (1+ current) (length source))
		   #\Nul
		   (elt source (1+ current))))

	     (str ()
	       (loop until (or (string= (peek) #\")
			       (is-at-end))
		     do (when (string= (peek) #\Newline)
			  (incf line))
			(advance))
	       (when (is-at-end)
		 (error "Unterminated string"))
	       (advance)  ;; eat the closing quote
	       (let ((value (subseq source (1+ start) (1- current))))
		 (add-token 'cl-lox/tokens:string value)))

	     (number ()
	       (loop while (digit-char-p (peek)) do (advance))
	       (when (and (string= (peek) #\.)
			  (digit-char-p (peek-next)))
		 (advance)  ;; consume the dot
		 (loop while (digit-char-p (peek)) do (advance)))
	       (let ((value (read-from-string (subseq source start current))))

		 (add-token 'cl-lox/tokens:number (float value))))

	     (is-alpha? (c)
	       (or (alpha-char-p c) (equals c #\_)))

	     (is-alphanumeric?  (c)
	       (or (is-alpha? c) (digit-char-p c)))

	     (identifier ()
	       (loop while (is-alphanumeric? (peek))
		     do (advance))

	       (let* ((text (subseq source start current))
		      (typepair (assoc text cl-lox/tokens:keywords
				       :test 'equals))
		      (type (if typepair (cdr typepair)
				'cl-lox/tokens:identifier)))

		 (add-token type)))

	     (scan-token ()
	       (let ((c (advance)))
		 (case c
		   (#\( (add-token 'cl-lox/tokens:left-paren))
		   (#\) (add-token 'cl-lox/tokens:right-paren))
		   (#\{ (add-token 'cl-lox/tokens:left-brace))
		   (#\} (add-token 'cl-lox/tokens:right-brace))
		   (#\, (add-token 'cl-lox/tokens:comma))
		   (#\- (add-token 'cl-lox/tokens:minus))
		   (#\+ (add-token 'cl-lox/tokens:plus))
		   (#\; (add-token 'cl-lox/tokens:semicolon))
		   (#\* (add-token 'cl-lox/tokens:star))
		   (#\! (add-token (if (match #\=)
				       'cl-lox/tokens:bang-equal
				       'cl-lox/tokens:equal)))
		   (#\= (add-token (if (match #\=)
				       'cl-lox/tokens:equal-equal
				       'cl-lox/tokens:equal)))
		   (#\< (add-token (if (match #\=)
				       'cl-lox/tokens:less-equal
				       'cl-lox/tokens:less)))
		   (#\> (add-token (if (match #\=)
				       'cl-lox/tokens:greater-equal
				       'cl-lox/tokens:greater)))
		   (#\/ (if (match #\/)
			    (loop until (or (string= (peek) #\Newline)
					    (is-at-end))
				  do (advance))
			    (add-token 'cl-lox/tokens:slash)))

		   ;; Note #\Return is the Windows \r character
		   ((#\Space #\Return #\Tab)
		    ;; Do nothing
		    )

		   (#\Newline (incf line))

		   (#\" (str))

		   (t
		    (cond ((digit-char-p c)
			   (number))
			  ((is-alpha? c)
			   (identifier))
			  (t
			   (error "Shouldn't get here; unrecognized character"))))))))
      (loop until (is-at-end)
	    do (setf start current)
	       (scan-token)))
    (push (make-token 'cl-lox/tokens:eof "" nil line) tokens)
    (nreverse tokens)))
