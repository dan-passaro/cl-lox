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

(defun run-and-capture (lox-source)
  (let ((*standard-output* (make-string-output-stream))
	(*error-output* (make-string-output-stream)))
    (cl-lox:run lox-source)
    (lox-result (get-output-stream-string *standard-output*)
		(get-output-stream-string *error-output*))))

(test print-statement
  (let ((result (run-and-capture "print \"Hello\";")))
    (is (string= (format nil "Hello~%")
		 (stdout result)))
    (is (string= "" (stderr result)))))
