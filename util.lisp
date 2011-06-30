;; Copyright (c) 2011, Krzysztof Drewniak
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;     * Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;     * Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in the
;;       documentation and/or other materials provided with the distribution.
;;     * Neither the name of cl-pong nor the
;;       names of its contributors may be used to endorse or promote products
;;       derived from this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
