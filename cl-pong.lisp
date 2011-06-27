(in-package #:cl-pong)

(defvar *game* nil)

(defvar *score* '(0 0))

(defgeneric draw (thing))
(defgeneric move (thing amount))
(defgeneric reflect-p (thing ball))
(defgeneric reflect (thing ball))

(defclass game ()
  ((left :initform (make-instance 'paddle
				  :x +paddle-width+
				  :y (/ +screen-height+ 2)
				  :extent 30)
	 :accessor left)
   (right :initform (make-instance 'paddle
				   :x (- +screen-width+ +paddle-width+)
				   :y (/ +screen-height+ 2)
				   :extent 30)
	  :accessor right)
   (reflectables :initform nil :accessor reflectables)
   (ball :initform (make-instance 'ball 
				  :x (/ +screen-width+ 2)
				  :y (/ +screen-height+ 2)
				  :r 8
				  :angle 60)
	 :accessor ball)
   (game-p :initform nil :accessor game-p)))

(defclass reflectable () ())

(defclass wall (reflectable position) 
  ((wall-edge :initarg :wall-edge :accessor wall-edge)))

(defclass score-area (wall) ())

(defclass position ()
  ((x :initarg :x :accessor x) (y :initarg :y :accessor y)))

(defclass paddle (position reflectable)
  ((extent :initarg :extent :accessor extent)))

(defclass ball (position)
  ((angle :initarg :angle :accessor angle)
   (r :initarg :r :accessor r)))

(defmethod initialize-instance :after ((self game) &key &allow-other-keys)
  (setf (reflectables self)
	(list
	 (left self) (right self)
	 (make-instance 'wall :y 0 :wall-edge #'y)
	 (make-instance 'wall :y +screen-height+ :wall-edge #'y)
	 (make-instance 'score-area :x 0 :wall-edge #'x)
	 (make-instance 'score-area :x +screen-width+ :wall-edge #'x))))

(defmethod reflect-p ((game game) (ball ball))
  (loop for i in (reflectables game) do
       (when (reflect-p i ball) (reflect i ball) (return-from reflect-p t))))

(defmethod draw ((game game))
  (fill-surface *black*)
  (draw (left game))
  (draw (right game))
  (draw (ball game))
  (draw-string-solid-* (format nil "~{~d~^-~}" *score*)
			   (/ +screen-width+ 2) 1 :justify :center))

(defmethod move ((position ball) amount)
  (with-accessors ((x x) (y y) (angle angle)) position
    (let ((apply (angle->xy angle amount)))
      (rangef x 0 +screen-width+ (first apply)) 
      (rangef y 0 +screen-height+ (- (second apply))))))

(defmethod reflect-p ((wall wall) (ball ball))
  (let ((edge (wall-edge wall)))
    (= (funcall edge wall)
       (funcall (if (= (funcall edge wall) 0) #'- #'+)
		(round (funcall edge ball)) (r ball)))))
 
(defmethod reflect ((wall wall) (ball ball))
  (setf (angle ball) (reflect-axis :x (angle ball))))

(defmethod reflect ((wall score-area) (ball ball))
  (if (= (x wall) 0)
      (incf (second *score*))
      (incf (first *score*)))
  (setf *game* (make-instance 'game)))

(defmethod reflect-p ((paddle paddle) (ball ball))
  (and 
   (if (< (angle ball) 180)
       (= (x paddle) (+ (round (x ball)) (r ball)))
       (= (x paddle) (- (round (x ball)) (r ball))))
   (<= (- (y paddle) (extent paddle)) (y ball) (+ (y paddle) (extent paddle)))))

(defmethod reflect ((paddle paddle) (ball ball))
  (setf (angle ball) (reflect-axis :y (angle ball)))
  (setf (angle ball) (if (>= (angle ball) 180) 180 0))
  (let* ((half (< (y ball) (y paddle)))
	 (divisor (/ (abs (- (y ball) (y paddle)))
		      (abs (extent paddle))))
	 (angle (round (+ 20 (* 70 (float divisor))))))
    (incf (angle ball) (+ angle (if half
				    (if (>= (angle ball) 180) 90 0)
				    (if (>= (angle ball) 180) 0 90))))))

(defmethod move ((paddle paddle) amount)
  (rangef (y paddle) 0 +screen-height+ amount))

(defmethod draw ((paddle paddle))
  (draw-box-* (x paddle) (- (y paddle) (extent paddle)) 
	      +paddle-width+ (1+ (* 2 (extent paddle)))))

(defmethod draw ((ball ball))
  (draw-filled-circle-* (round (x ball)) (round (y ball)) (r ball)))

(defun process-key (key game)
  (if (not (game-p game))
      (case key
	(:sdl-key-q (push-quit-event))
	(t (setf (game-p game) t)))
      (case key
	((:sdl-key-w :sdl-key-a) (move (left game) -2))
	((:sdl-key-s :sdl-key-d) (move (left game) 2))
	((:sdl-key-up :sdl-key-left) (move (right game) -2))
	((:sdl-key-down :sdl-key-right) (move (right game) 2))
	(:sql-key-q (push-quit-event)))))

(defun main ()
  (with-init (sdl-init-video)
    (window +screen-width+ +screen-height+ :title-caption "PONG!")
    (update-display)
    (initialise-default-font *font-10x20*)
    (setf (frame-rate) 60)
    (enable-key-repeat 16 16)
    (setf *game* (make-instance 'game))
    (with-color (col *white*)
      (with-events (:poll)
	(:quit-event () t)
	(:key-down-event 
	 (:key key)
	 (loop named start-game do
	      (process-key key *game*)
	      (when (game-p *game*) (return-from start-game t))))
	(:idle
	 ;; TODO: Add AI here
	 (when (game-p *game*)
	   (reflect-p *game* (ball *game*))
	   (if(game-p *game*)
	      (move (ball *game*) 1)))
	 (draw *game*)
	 (update-display))))))
