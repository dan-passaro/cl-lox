(defpackage :cl-lox/equals
  (:export :equals)
  (:documentation "Provide EQUALS, an CLOS-compatible equality function.")
  (:use :cl))
(in-package :cl-lox/equals)

(defgeneric equals (a b)
  (:documentation "Return if A and B are equal.

Lists are EQUALS if all their elements are EQUALS.

Defaults to EQUALP."))

(defmethod equals (a b)
  (equalp a b))

(defmethod equals ((a list) (b list))
  (or (and (null a) (null b))  ;; recursive base case
      (and (equals (car a) (car b))
	   (equals (cdr a) (cdr b)))))

(defmethod equals ((a structure-object) (b structure-object))
  (and (eq (class-of a) (class-of b))
       (every (lambda (slot)
		(equals (slot-value a (closer-mop:slot-definition-name slot))
			(slot-value b (closer-mop:slot-definition-name slot))))
	      (closer-mop:class-slots (class-of a)))))
