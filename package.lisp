;;;; package.lisp

(defpackage #:cl-pong
  (:use #:cl #:lispbuilder-sdl)
  (:shadow #:x #:y #:position)
  (:export #:main))
