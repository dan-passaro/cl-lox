(defpackage :cl-lox/main
  (:nicknames :cl-lox)
  (:use :cl)
  (:export :run :run-prompt :run-file))
(in-package :cl-lox/main)

(defun run (code-str)
  "Parse and then execute code-str."

  ;; For now, hardcoded to assume a one-line "print" instruction. MVP :)
  (let* ((first-quote-pos (position #\" code-str))
	 (second-quote-pos (position #\" code-str :start (1+ first-quote-pos)))
	 (str-content (subseq code-str (1+ first-quote-pos) second-quote-pos)))
    (format t "~a~%" str-content)))

(defun println (text)
  (format t text)
  (princ #\Newline))

(defun run-file (path)
  "Open the Lox script at PATH and execute it."
  (let ((program-code (uiop:read-file-string path)))
    (run program-code)))

(defun run-prompt ()
  "Start an interactive Lox shell."
  (block prompt-loop
    (loop
      (princ "> ")
      (let ((line (read-line *standard-input* nil)))
	(when (not line) (return-from prompt-loop))
	(run line)))))

(defun main ()
  "Toplevel entry point for a cl-lox binary."
  (let ((args (uiop:command-line-arguments)))
    (when (> (length args) 1)
      (println "Usage: cl-lox [script]")
      (uiop:quit 64))
    (when (= (length args) 1)
      (run-file (first args)))
    (run-prompt)))
