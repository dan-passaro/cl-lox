@echo off
sbcl --noinform --non-interactive --eval "(ql:quickload :cl-lox :silent t)" --eval "(cl-lox:main)" %*
