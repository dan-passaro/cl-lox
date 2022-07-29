(defpackage :cl-lox/tests
  (:export :cl-lox-tests)

  (:use :cl :fiveam)
  (:import-from :str)
  (:import-from :cl-lox)
  (:import-from :cl-lox/test-evaluate)
  (:import-from :cl-lox/test-parse)
  (:import-from :cl-lox/test-print-lox-ast)
  (:import-from :cl-lox/test-suite :cl-lox-tests)
  (:import-from :cl-lox/equals :equals)
  (:import-from :cl-lox/scan)
  (:import-from :cl-lox/token :make-token)
  (:import-from :cl-lox/tokens))
(in-package :cl-lox/tests)

(in-suite cl-lox-tests)

(defclass lox-result ()
  ((stdout :accessor stdout)
   (stderr :accessor stderr)))

(defun lox-result (stdout stderr)
  (let ((result (make-instance 'lox-result)))
    (setf (stdout result) stdout)
    (setf (stderr result) stderr)
    result))

(defmethod print-object ((r lox-result) s)
  (format s "#.(~(~s~) ~s ~s)"
	  'lox-result
	  (stdout r)
	  (stderr r)))

(defparameter *eof-token* (make-token 'cl-lox/tokens:eof "" nil 1))

(defun capture-result (func &rest args)
  (let ((*standard-output* (make-string-output-stream))
	(*error-output* (make-string-output-stream)))
    (apply func args)
    (lox-result (get-output-stream-string *standard-output*)
		(get-output-stream-string *error-output*))))

(defun run-and-capture (lox-source)
  (setf cl-lox/report-error:*had-error* nil)
  (capture-result 'cl-lox:run (str:trim-left lox-source)))

(defun run-prompt-and-capture (&rest lines)
  (setf cl-lox/report-error:*had-error* nil)
  (with-input-from-string (*standard-input*
			   (apply 'join-lines lines))
    (capture-result 'cl-lox:run-prompt)))

(test print-statement
  (let ((result (run-and-capture "print \"Hello\";")))
    (is (string= (format nil "Hello~%")
		 (stdout result)))
    (is (string= "" (stderr result))))
  (let ((result (run-and-capture "print \"Howdy\";")))
    (is (string= (format nil "Howdy~%")
		 (stdout result)))
    (is (string= "" (stderr result))))
  (let ((result (run-and-capture "print 10.0;")))
    (is (equals (format nil "10~%") (stdout result)))
    (is (equals "" (stderr result)))))

(test run-file
  (uiop:with-temporary-file (:stream fstream :pathname path)
    (format fstream "print \"Hello from file\";~%")
    (finish-output fstream)
    (let ((result (capture-result 'cl-lox:run-file path)))
      (is (string= (format nil "Hello from file~%")
		   (stdout result)))
      (is (string= "" (stderr result))))))

(test run-prompt
  (let ((result (run-prompt-and-capture "print \"I'm in a prompt\";")))
    (is (string= (format nil "> I'm in a prompt~%> ")
		 (stdout result)))
    (is (string= "" (stderr result)))))

(test run-prompt-allows-variables
  (let ((result (run-prompt-and-capture
		 "var a = 3;"
		 "print a;")))
    (is (string= (format nil "> > 3~%> ")
		 (stdout result)))
    (is (string= "" (stderr result)))))

(test run-prompt-continues-after-error
  (let ((result (run-prompt-and-capture
		 "print 1;"
		 "print +;"
		 "print 2;")))
    (is (string= (format nil "> 1~%> > 2~%> ")
		 (stdout result)))
    (is (string= (join-lines "[line 1] Error at '+': Expect expression.")
		 (stderr result)))))

(test run-prompt-allows-expressions-without-semicolon
  (let ((result (run-prompt-and-capture "4 + 5")))
    (is (string= (format nil "> 9~%> ") (stdout result)))
    (is (string= "" (stderr result)))))

(test scans-numbers
  (is (equals (list (make-token 'cl-lox/tokens:number "3" 3.0 1)
		    (make-token 'cl-lox/tokens:plus "+" nil 1)
		    (make-token 'cl-lox/tokens:number "4" 4.0 1)
		    *eof-token*)
	      (cl-lox/scan:scan-tokens "3 + 4"))))

(test scans-identifiers
  (is (equals (list (make-token 'cl-lox/tokens:identifier "a" nil 1)
		    (make-token 'cl-lox/tokens:plus "+" nil 1)
		    (make-token 'cl-lox/tokens:identifier "b" nil 1)
		    *eof-token*)
	      (cl-lox/scan:scan-tokens "a + b")))

  (is (equals (list (make-token 'cl-lox/tokens:identifier "_myVar" nil 1)
		    (make-token 'cl-lox/tokens:plus "+" nil 1)
		    (make-token 'cl-lox/tokens:identifier "num100" nil 1)
		    *eof-token*)
	      (cl-lox/scan:scan-tokens "_myVar + num100"))))

(test scans-keywords
  (is (equals (list (make-token 'cl-lox/tokens:class "class" nil 1)
		    *eof-token*)
	      (cl-lox/scan:scan-tokens "class")))
  (is (equals (list (make-token 'cl-lox/tokens:false "false" nil 1)
		    *eof-token*)
	      (cl-lox/scan:scan-tokens "false"))))

(test scans-bang
  (is (equals (list (make-token 'cl-lox/tokens:bang "!" nil 1)
		    (make-token 'cl-lox/tokens:number "5" 5.0 1)
		    *eof-token*)
	      (cl-lox/scan:scan-tokens "!5"))))

(test scans-semicolon
  (is (equals (list (make-token 'cl-lox/tokens:print "print" nil 1)
		    (make-token 'cl-lox/tokens:string "\"hello\"" "hello" 1)
		    (make-token 'cl-lox/tokens:semicolon ";" nil 1)
		    *eof-token*)
	      (cl-lox/scan:scan-tokens "print \"hello\";"))))

(test assigns-toplevel-variables
  (let ((result (run-and-capture "
var a = 1;
var b = 2;
print a + b;
")))
    (is (string= (format nil "3~%")
		 (stdout result)))
    (is (string= "" (stderr result)))))

(test errors-on-accessing-undefined-variable
  (let ((result (run-and-capture "print a;")))
    (is (string= "" (stdout result)))
    (is (string= (format nil "Undefined variable 'a'.~%[line 1]~%")
		 (stderr result)))))

(test errors-on-negating-string
  (let ((result (run-and-capture "print -\"foo\";")))
    (is (string= "" (stdout result)))
    (is (string= (format nil "Operand must be a number.~%[line 1]~%")
		 (stderr result)))))

(test reports-syntax-errors
  (let ((result (run-and-capture "+-;")))
    (is (string= "" (stdout result)))
    (is (string= (join-lines
		  "[line 1] Error at '+': Expect expression.")
		 (stderr result))))
  (let ((result (run-and-capture "(3;")))
    (is (string= "" (stdout result)))
    (is (string= (format nil "[line 1] Error at ';': Expect ')' after expression.~%")
		 (stderr result)))))

(defun join-lines (&rest lines)
  (format nil "~{~a~%~}" lines))

(test synchronizes-after-parse-error
  (let ((result (run-and-capture "print 3++5-*\"hello\";
 print 5;
 print (4;")))
    (is (string= "" (stdout result)))
    (is (string= (join-lines
		  "[line 1] Error at '+': Expect expression."
		  "[line 3] Error at ';': Expect ')' after expression.")
		 (stderr result)))))

(test negating-true-gives-false
  (let ((result (run-and-capture "print !true;")))
    (is (string= (join-lines "false") (stdout result)))
    (is (string= "" (stderr result)))))

;; TODO: ensure scanner errors are reported properly (i.e. test
;; unterminated string behavior)

;; TODO: ensure all binary operators typecheck appropriately
