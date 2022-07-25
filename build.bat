@echo off
sbcl --non-interactive --eval "(asdf:make :cl-lox/executable)"
