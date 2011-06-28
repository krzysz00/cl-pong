(in-package #:cl-pong)

(defconstant +screen-width+ 800)
(defconstant +screen-height+ 600)
(defconstant +paddle-width+ 16)

(defun %angle->xy (angle amount primary-x)
  (let ((dev (* amount (float (/ angle 90)))))
    (values (if primary-x (- 1 dev) dev) 
	    (if primary-x dev (- 1 dev)))))

(defun angle->xy (angle amount)
  (let ((angle (mod angle 360)))
    (multiple-value-bind (x y) 
	(%angle->xy (mod angle 90) amount (or (< 89 angle 180) (< 269 angle 360)))
      (cond
	((< angle 90) (list x y))
	((< 89 angle 180) (list x (- y)))
	((< 179 angle 270) (list (- x) (- y)))
	((< 269 angle 360) (list (- x) y))))))

(defun reflect-axis (axis angle)
  (if (eql axis :y)
      (cond
	((< angle 90) (+ (- 90 angle) 270))
	((< 89 angle 180) (+ (- 180 angle) 180))
	((< 179 angle 270) (+ (- 270 angle) 90))
	((< 269 angle 360) (- 360 angle)))
      (cond
	((< angle 90) (+ (- 90 angle) 90))
	((< 89 angle 180) (- 180 angle))
	((< 179 angle 270) (+ (- 270 angle) 270))
	((< 269 angle 360) (+ (- 360 angle) 180)))))

(defun %rangef (place min max &optional (delta 1)) 
  (min max (max min (+ place delta))))

(define-modify-macro rangef (min max &optional (delta 1)) %rangef)
