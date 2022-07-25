(defpackage :cl-lox/tokens
  (:export
   :left-paren
   :right-paren
   :left-brace
   :right-brace
   :comma
   :dot
   :minus
   :plus
   :semicolon
   :star
   :bang
   :bang-equal
   :equal
   :equal-equal
   :less
   :less-equal
   :greater
   :greater-equal
   :slash
   :string
   :number
   :identifier
   :class
   :keywords
   :and
   :class
   :else
   :false
   :for
   :fun
   :if
   :nil
   :or
   :print
   :return
   :super
   :this
   :var
   :while)
  (:use :cl)
  (:shadow :and :if :nil :or :print :equal :number :string))
(in-package :cl-lox/tokens)

(defparameter left-paren "(")
(defparameter right-paren ")")
(defparameter left-brace "{")
(defparameter right-brace "}")
(defparameter comma ",")
(defparameter dot ".")
(defparameter minus "-")
(defparameter plus "+")
(defparameter semicolon ";")
(defparameter star "*")
(defparameter bang "!")
(defparameter bang-equal "!=")
(defparameter equal "=")
(defparameter equal-equal "==")
(defparameter less "<")
(defparameter less-equal "<=")
(defparameter greater ">")
(defparameter greater-equal ">=")
(defparameter slash "/")
(defparameter string "STRING")
(defparameter number "NUMBER")
(defparameter identifier "IDENTIFIER")

(defparameter keywords
  '(("and" . and)
    ("class" . class)
    ("else" . else)
    ("false" . false)
    ("for" . for)
    ("fun" . fun)
    ("if" . if)
    ("nil" . nil)
    ("or" . or)
    ("print" . print)
    ("return" . return)
    ("super" . super)
    ("this" . this)
    ("var" . var)
    ("while" . while)))
