(defpackage :cl-lox/tests
  (:use :cl :fiveam)
  (:import-from :cl-lox)
  (:export :cl-lox-tests))
(in-package :cl-lox/tests)

(def-suite* cl-lox-tests
  :description "Tests for CL-LOX")

(defclass lox-result ()
  ((stdout :accessor stdout)
   (stderr :accessor stderr)))

(defun lox-result (stdout stderr)
  (let ((result (make-instance 'lox-result)))
    (setf (stdout result) stdout)
    (setf (stderr result) stderr)
    result))

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
    (is (string= "" (stderr result)))))

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
