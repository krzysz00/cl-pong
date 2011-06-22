;;;; cl-pong.asd

(asdf:defsystem #:cl-pong
  :serial t
  :depends-on (:lispbuilder-sdl)
  :components ((:file "package")
               (:file "cl-pong")))

