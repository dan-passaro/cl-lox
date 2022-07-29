(defpackage :cl-lox/main
  (:nicknames :cl-lox)
  (:use :cl)
  (:import-from :cl-lox/environment :make-environment)
  (:import-from :cl-lox/evaluate :evaluate)
  (:import-from :cl-lox/interpret :interpret :stringify)
  (:import-from :cl-lox/parse :parse)
  (:import-from :cl-lox/scan :scan-tokens)
  (:import-from :cl-lox/stmt :expression-stmt-expression)
  (:import-from :cl-lox/token :make-token)
  (:import-from :cl-lox/report-error :*had-error*)
  (:import-from :cl-lox/print-lox-ast :print-lox-ast)
  (:export :main :make-token :print-lox-ast :run :run-prompt :run-file))
(in-package :cl-lox/main)

(defun run (code-str &optional environment)
  "Parse and then execute code-str."
  (let ((code-ast (parse (scan-tokens code-str))))
    (unless *had-error* (interpret code-ast environment))))

(defun println (text)
  (format t "~a~%" text))

(defun run-file (path)
  "Open the Lox script at PATH and execute it."
  (let ((program-code (uiop:read-file-string path)))
    (run program-code)))

(defun run-prompt ()
  "Start an interactive Lox shell."
  (let ((lox-environment (make-environment)))
    (loop
      (setf cl-lox/report-error:*had-error* nil)
      (princ "> ")
      (finish-output)

      (let* ((line (read-line *standard-input* nil))
	     (line-empty (or (not line) (string= (str:trim line) ""))))
	(when line-empty (return-from run-prompt))
	(if (str:ends-with? ";" line)

	    (run line lox-environment)

	    ;; allow running an expression with no semicolon and
	    ;; printing the result
	    (let* ((expr-stmt-str (format nil "~a;" line))
		   (expr-stmt (first (parse (scan-tokens expr-stmt-str))))
		   (result (evaluate (expression-stmt-expression expr-stmt)
				     lox-environment)))
	      (println (stringify result))))))))

(defun main ()
  "Toplevel entry point for a cl-lox binary."
  (let ((args (uiop:command-line-arguments)))
    (cond ((> (length args) 1)
	   (println "Usage: cl-lox [script]")
	   (uiop:quit 64))
	  ((= (length args) 1)
	   (run-file (first args)))
	  (t
	   (run-prompt)))))
