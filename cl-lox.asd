#-asdf3.1 (error "CL-LOX requires ASDF 3.1 or later.")
(asdf:defsystem "cl-lox"
  :class :package-inferred-system
  :depends-on ("cl-lox/main")
  :description "Describe cl-lox here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t)

(asdf:defsystem "cl-lox/executable"
  :build-operation program-op
  :build-pathname "cllox"
  :depends-on ("cl-lox")
  :entry-point "CL-LOX:MAIN")
