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
   :identifier)
  (:use :cl)
  (:shadow :equal :number :string))
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
