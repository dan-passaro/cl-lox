(defpackage :cl-lox/tests
  (:export :cl-lox-tests)
  (:use :cl :fiveam)
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

(defparameter *eof-token* (make-token 'cl-lox/tokens:eof "" nil 1))

(defun capture-result (func &rest args)
  (let ((*standard-output* (make-string-output-stream))
	(*error-output* (make-string-output-stream)))
    (apply func args)
    (lox-result (get-output-stream-string *standard-output*)
		(get-output-stream-string *error-output*))))

(defun run-and-capture (lox-source)
  (capture-result 'cl-lox:run lox-source))

(test print-statement
  (let ((result (run-and-capture "print \"Hello\";")))
    (is (string= (format nil "Hello~%")
		 (stdout result)))
    (is (string= "" (stderr result))))
  (let ((result (run-and-capture "print \"Howdy\";")))
    (is (string= (format nil "Howdy~%")
		 (stdout result)))
    (is (string= "" (stderr result))))
  ;; (let ((result (run-and-capture "print 10.0;")))
  ;;   (is (string= (format nil "10~%")))
  ;;   (is (string= "" (stderr result))))
  )

(test run-file
  (uiop:with-temporary-file (:stream fstream :pathname path)
    (format fstream "print \"Hello from file\";~%")
    (finish-output fstream)
    (let ((result (capture-result 'cl-lox:run-file path)))
      (is (string= (format nil "Hello from file~%")
		   (stdout result)))
      (is (string= "" (stderr result))))))

(test run-prompt
  (with-input-from-string (*standard-input*
			   (format nil "print \"I'm in a prompt\";~%"))
    (let ((result (capture-result 'cl-lox:run-prompt)))
      (is (string= (format nil "> I'm in a prompt~%> ")
		   (stdout result)))
      (is (string= "" (stderr result))))))

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
