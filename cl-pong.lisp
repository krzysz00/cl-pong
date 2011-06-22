(in-package #:cl-pong)

(defun %angle->xy (angle amount)
  (let ((dev (float 
