;;;; Asteroids

;;; utilities

;;; from PCL
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

;;; pg's anaphoric if
(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form))) ;Q. Why let around the else form?

;;; check the performance of this against push nreverse- might be pointless
(defun accumulate (next fn list)
  "Repeatedly append to a list without having to traverse the cdrs"
  (let ((path (cons nil nil)))
    (labels ((acc (end rest)
	       (if (null rest)
		   (cdr path)
		   (acc (setf (cdr end)
			      (cons (funcall fn rest) nil)) (funcall next rest)))))
      (acc path list))))

(defun map0-n (fn n) (mapa-b fn 0 n))
(defun map1-n (fn n) (mapa-b fn 1 n))

(defun mapa-b (fn a b &optional (step 1))
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))

;;; todo find out if this recursive map is already defined

(defun rmapcar (fn list)
  (mapcar #'(lambda (item)
	      (if (listp item)
		  (rmapcar fn item)
		  (funcall fn item))) list))

;;; constants

(defconstant 2pi (* 2 pi))

;;; fixed parameters

(defparameter *modulo-offset* 1)
(defparameter *modulo-size* 2)
(defparameter *asteroid-vertices* 23)
(defparameter *asteroid-vertex-angle* (/ 2pi *asteroid-vertices*))
(defparameter *asteroid-crustiness* 0.40)
(defparameter *asteroid-min-radius* 0.05)
(defparameter *asteroid-max-radius* 0.20)
(defparameter *scale* 0.023)
(defparameter *digit-width* (* *scale* 2.4))
(defparameter *digit-height* (* *scale* 3.4))
(defparameter *edge* 0.01)
(defparameter *left* (+ -1.0 *edge*))
(defparameter *right* (- 1.0 *edge*))
(defparameter *top* (- 1.0 *edge*))
(defparameter *bottom* (+ -1.0 *edge*))
(defparameter *ship-width* (* 2 *scale*))
(defparameter *spacing* (* 0.4 *scale*))
(defparameter *asteroid-max-speed* 0.01) ; in one direction
(defparameter *asteroid-min-omega* 0)
(defparameter *asteroid-max-omega* 0.05)
(defparameter *asteroid-min-split-radius* 0.04)
(defparameter *bullet-speed* 0.04)
(defparameter *ship-omega* 0.15)
(defparameter *max-ship-speed* 0.03) 
(defparameter *thrust* 0.0007)
(defparameter *bullet-duration* 35)
(defparameter *ship-effective-radius* *scale*)
(defparameter *starting-lives* 3)
(defparameter *ship-delay* 75)
(defparameter *asteroid-create-delay* 35)
(defparameter *post-game-delay* 125)
(defparameter *force-field-delay* 125) ;> ship delay
(defparameter *score-digits* 5)
(defparameter *hi-score-switch-delay* 150)
(defparameter *max-scores* 6)
(defparameter *shot-sound* nil)
(defparameter *explosion-sounds* nil)
(defparameter *ufo-ping-sounds* nil)
(defparameter *music-files* (list "sounds/music1.ogg" "sounds/music2.ogg" "sounds/music3.ogg"))
(defparameter *ufo-radius* (* 2.2 *scale*))
(defparameter *ufo-period* 720)

;;; essential state

(defstruct (point (:type vector) (:constructor make-point (x y))) x y)

(defparameter *frame* 0)
(defparameter *ship-angle* 0)
(defparameter *ship-velocity* (make-point 0 0))
(defparameter *ship-position* (make-point 0 0))
(defparameter *ship-position-reset* 0)
(defparameter *life-start* 0)
(defparameter *score* 0)
(defparameter *lives* 0)
(defparameter *asteroids* nil)
(defparameter *bullets* nil)
(defparameter *actions* (make-hash-table))
(defparameter *level* 0)
(defparameter *next-wave* 0)
(defparameter *hi-scores* (list
			   (cons 127 "CSD")
			   (cons 116 "DAN")
			   (cons 84 "EMM")
			   (cons 78 "ROB")
			   (cons 54 "HEN")
			   (cons 12 "HEL")))
(defparameter *ufo-start* nil)
(defparameter *ufo-position* nil)
(defparameter *ufo-velocity* nil)
(defparameter *ufo-strength* nil)
(defparameter *new-name* nil)
(defparameter *new-hiscore-position* nil)
(defparameter *new-name-cursor* 0)

;;; shape stuff

(defun make-path-from-list (coordinates)
  (accumulate #'cddr #'(lambda (x) (make-point (car x) (cadr x))) coordinates))

(defmacro with-sine-and-cosine (name radians &body body)
  (with-gensyms (angle-value-name)
    `(let* ((,angle-value-name ,radians)
	    (,(intern (format nil "COS-~a" name)) (cos ,angle-value-name))
	    (,(intern (format nil "SIN-~a" name)) (sin ,angle-value-name)))
       ,@body)))

(defun rand-between (a b)
  (if (< a b)
      (+ a (random (- b a)))
     (+ b (random (- a b)))))

(defun make-shape (&rest coordinate-lists)
  (mapcar #'make-path-from-list coordinate-lists))

(defun make-shape-from-points (&rest point-lists) point-lists)

(defun rotate (point cos-theta sin-theta)
    (make-point (- (* (point-x point) cos-theta) (* (point-y point) sin-theta))
		(+ (* (point-x point) sin-theta) (* (point-y point) cos-theta))))

(defun rotate-point (point angle)
  (with-sine-and-cosine theta angle
    (rotate point cos-theta sin-theta)))

(defun rotate-list (list angle)
  (with-sine-and-cosine theta angle
    (rmapcar #'(lambda (point) (rotate point
				       cos-theta
				       sin-theta)) list)))

;;todo use this- some functions aren't
(defun translate-point (p1 p2)
  (make-point (+ (point-x p1) (point-x p2))
	      (+ (point-y p1) (point-y p2))))

(defun translate (point x-offset y-offset)
  (make-point (+ (point-x point) x-offset)
	      (+ (point-y point) y-offset)))

(defun translate-list (list x-offset y-offset)
  (rmapcar #'(lambda (point) (translate point
					x-offset
					y-offset)) list))
(defun translate-list-by-point (list point)
  (translate-list list (point-x point) (point-y point)))


(defun scale (point scale-factor)
  (make-point (* (point-x point) scale-factor)
	      (* (point-y point) scale-factor)))

(defun scale-list (list scale-factor)
  (rmapcar #'(lambda (point) (scale point scale-factor)) list))

(defun make-scaled-shape (&rest coordinates)
  (scale-list (apply #'make-shape coordinates) *scale*))

(defparameter *digits*
  (vector (make-scaled-shape '(0 0 2 0 2 -3 0 -3 0 0))
	  (make-scaled-shape '(0 -3 2 -3) '(1 -3 1 0 0.5 -0.5))
	  (make-scaled-shape '(0.2 0 2 0 2 -1.5 0 -1.5 0 -3 2 -3))
	  (make-scaled-shape '(0 0 2 0 2 -3 0 -3) '(0 -1.5 2 -1.5))
	  (make-scaled-shape '(0 0 0 -1.5 2 -1.5) '(2 0 2 -3))
	  (make-scaled-shape '(2 0 0 0 0 -1.5 2 -1.5 2 -3 0.2 -3))
	  (make-scaled-shape '(0 0 0 -3 2 -3 2 -1.5 0 -1.5))
	  (make-scaled-shape '(0 0 2 0 2 -3))
	  (make-scaled-shape '(0 0 2 0 2 -3 0 -3 0 0) '(0 -1.5 2 -1.5))
	  (make-scaled-shape '(2 -3 2 0 0 0 0 -1.5 2 -1.5))))

(defparameter *letters*
  (let ((table (make-hash-table)))
    (labels ((make-letter (char &rest coord-lists)
	       (setf (gethash char table) (apply #'make-scaled-shape coord-lists))))
      (make-letter #\a '(0 -3 0 -1 1 0 2 -1 2 -3) '(0 -1.5 2 -1.5))
      (make-letter #\b '(0 0 1 0 2 -0.75 1 -1.5 2 -2.25 1 -3 0 -3 0 0) '(0 -1.5 1 -1.5))
      (make-letter #\c '(2 0 1 0 0 -1 0 -2 1 -3 2 -3))
      (make-letter #\d '(0 0 1 0 2 -1 2 -2 1 -3 0 -3 0 0))
      (make-letter #\e '(2 0 0 0 0 -3 2 -3) '(0 -1.5 2 -1.5))
      (make-letter #\f '(2 0 0 0 0 -3) '(0 -1.5 2 -1.5))
      (make-letter #\g '(2 0 1 0 0 -1 0 -2 1 -3 2 -3 2 -1.5 1 -1.5))
      (make-letter #\h '(0 0 0 -3) '(0 -1.5 2 -1.5)'(2 0 2 -3))
      (make-letter #\i '(0 0 2 0) '(1 0 1 -3) '(0 -3 2 -3))
      (make-letter #\j '(0 0 2 0) '(1 0 1 -2 0 -3))
      (make-letter #\k '(0 0 0 -3) '(2 0 0 -1.5 2 -3))
      (make-letter #\l '(0 0 0 -3 2 -3))
      (make-letter #\m '(0 -3 0 0 1 -1 2 0 2 -3))
      (make-letter #\n '(0 -3 0 0 2 -3 2 0))
      (make-letter #\o '(1 0 0 -1 0 -2 1 -3 2 -2 2 -1 1 0))
      (make-letter #\p '(0 -3 0 0 1 0 2 -1 1 -2 0 -2))
      (make-letter #\q '(1 0 0 -1 0 -2 1 -3 2 -2 2 -1 1 0) '(1 -2 2 -3))
      (make-letter #\r '(0 -3 0 0 1 0 2 -1 1 -2 2 -3) '(0 -2 1 -2))
      (make-letter #\s '(2 0 0 0 0 -1.5 2 -1.5 2 -3 0 -3))
      (make-letter #\t '(0 0 2 0) '(1 0 1 -3))
      (make-letter #\u '(0 0 0 -3 2 -3 2 0))
      (make-letter #\v '(0 0 0 -2 1 -3 2 -2 2 0))
      (make-letter #\w '(0 0 0 -3 1 -2 2 -3 2 0))
      (make-letter #\x '(0 0 2 -3) '(2 0 0 -3))
      (make-letter #\y '(0 0 0 -1 1 -2 2 -1 2 0) '(1 -2 1 -3))
      (make-letter #\z '(0 0 2 0 0 -3 2 -3))
      (make-letter #\  ))
    table))


(defparameter *ufo-start-velocity* (make-point -0.01 -0.002))

(defparameter *ufo-shape*

  (rotate-list (make-scaled-shape '(0 -2 -.4 -1.8 -.7 -1.7 -.9 -1.3 -.8 -.7 -1.2 -.6
				    -1.6 -0.4 -2.0 0 -1.6 .4 -2.0 0 -1.6 .4 -1.2 .6 -.4 0.9
				    0 1 .4 .9 1.2 .6 1.6 .4 2.0 0 1.6 -.4 1.2 -.6 .8 -.7
				    .9 -1.3 .7 -1.7 .4 -1.8 0 -2)
				  '(-0.7 -1.7 -1.5 -2.5)
				  '(.7 -1.7 1.5 -2.5)
				  '(-.8 -.7 -.4 -.9 0 -1.0 .4 -.9 .8 -.7))
	       pi))
(defun get-digit (d) (elt *digits* d))

(defparameter *spaceship-shape* (make-scaled-shape '(0 1.5 -1 -1.5 0 -0.5 1 -1.5 0 1.5)))
(defparameter *ship-nose* (caar *spaceship-shape*))

(defun modulo (x)
  (- (rem (+ x *modulo-offset*) *modulo-size*) *modulo-offset*))

(defun rebound (x)
  (if (> x 0)
      (modulo x)
      (- (modulo (- x)))))
   
(defun close-list (list)
  (append list (list (car list))))  ;;this really sucks monkey balls

(defun make-circle (vertices radius-fn)
  (let ((angle (/ 2pi vertices)))
    (make-shape-from-points
     (close-list
      (map1-n (lambda (vertex) (rotate-point
				(make-point 0 (funcall radius-fn))
				(* vertex angle))) vertices)))))

(defun make-asteroid-shape (radius)
  (make-circle *asteroid-vertices*
	       (let ((min (* radius *asteroid-crustiness*)))
	       #'(lambda () (rand-between min radius)))))

(defun make-word-shape (word)
  (labels ((add (rest x shape)
	   (if (endp rest)
	       (values shape x)
	       (add (cdr rest) (+ x *digit-width*)
		    (append (translate-list (gethash (car rest) *letters*) x 0) shape)))))
    (add (map 'list #'char-downcase word) 0 nil)))

(defun make-number-shape (number digits)
  (labels ((add (n d x shape)
	       (if (zerop d)
		   shape
		   (multiple-value-bind (q m)
		       (floor n 10)
		     (add q (1- d) (- x *digit-width*)
			  (append (translate-list (get-digit m) x 0) shape))))))
    (add number digits (* (1- digits) *digit-width*) nil)))

(defstruct (bullet (:type vector)) start collided origin angle)
(defstruct (asteroid (:type vector)) start collided shape radius velocity origin omega)
(defstruct (action (:type vector)) start end type)

;;; Todo make this frame counter actual frames per second
;;; replace all (most) usages of *frame* with get-time

(defun get-time () *frame* (/ *frame* 60))

(defun draw-shapes (shapes) (mapc #'(lambda (shape) (draw-shape shape)) shapes))
(defun make-score () (translate-list (make-number-shape *score* *score-digits*) *left* *top*))
(defun make-level () (translate-list (make-number-shape *level* 2) (- *digit-width*) *top*))
(defun make-lives () (translate-list (make-lives-shape *lives*) *right* *top*))

;;; need to refactor this with the thing that makes the numbers
;;; in preparation for a type-set function which just concatenates
;;; arbitrary shapes so we can display messages.

(defun make-lives-shape (lives)
  (labels ((add (lives x shape)
	     (if (<= lives 0)
		 shape
		 (add (1- lives) (- x *ship-width* *spacing*)
		      (append (translate-list *spaceship-shape* 
					      x
					      (- (point-y *ship-nose*))) shape)))))
	   (add lives (* -0.5 *ship-width*) nil)))

(defun velocity-transform-point (pos v dt)
  (make-point
   (rebound (+ (point-x pos) (* dt (point-x v))))
   (rebound (+ (point-y pos) (* dt (point-y v))))))

(defun velocity-transform (shapes pos start v)
  (let ((dt (- *frame* start)))
    (translate-list-by-point shapes (velocity-transform-point pos v dt))))

(defun make-asplosion (start radius)
  (make-circle 8 #'(lambda () (* (/ radius 7) (- *frame* start)))))

(defun ufo-position ()
  (velocity-transform-point *ufo-position* *ufo-velocity* (- *frame* *ufo-start*)))
;again, this is duplication of effort, we should be able to pass the result of this into
;the transform- probably the velocity transform function is superfluous.
(defun make-ufo ()
  (if *ufo-start* (velocity-transform
		    *ufo-shape*
		    *ufo-position*
		    *ufo-start*
		    *ufo-velocity*)))

;;;todo this is really making the shapes for drawing-
;;;really should try to make the distinction in the
;;;names
(defun make-asteroids ()
  (mapcar #'(lambda (asteroid)
	      (if (asteroid-collided asteroid)
		  (velocity-transform
		   (make-asplosion (asteroid-collided asteroid) (asteroid-radius asteroid))
		   (asteroid-origin asteroid)
		   (asteroid-start asteroid)
		   (asteroid-velocity asteroid))
		  (velocity-transform
		   (rotate-list (asteroid-shape asteroid)
				(* (- (asteroid-start asteroid) *frame*) (asteroid-omega asteroid)))
		   (asteroid-origin asteroid)
		   (asteroid-start asteroid)
		   (asteroid-velocity asteroid))))
	  *asteroids*))

(defparameter *bullet-shape* (make-shape (list 0 0 0 (* 1.0 *scale*))))

(defun asteroid-position (asteroid)
  (velocity-transform-point
   (asteroid-origin asteroid)
   (asteroid-velocity asteroid)
   (- *frame* (asteroid-start asteroid))))

(defun ship-position ()
  (velocity-transform-point
   *ship-position*
   *ship-velocity*
   (- *frame* *ship-position-reset*)))

(defun bullet-position (bullet)
  (velocity-transform-point (bullet-origin bullet)
			    (rotate-point (make-point 0 *bullet-speed*)
					  (bullet-angle bullet))
			    (- *frame* (bullet-start bullet))))
;;;refactor this so that the following uses the preceding
(defun make-bullets ()
  (mapcar #'(lambda (bullet)
	      (velocity-transform
	       (rotate-list *bullet-shape* (bullet-angle bullet))
	       (bullet-origin bullet)
	       (bullet-start bullet)
	       (rotate-point (make-point 0 *bullet-speed*)
			     (bullet-angle bullet))))
	      *bullets*))

;;; add a new random asteroid to the list
;;; todo move the start initialiser to the default init

(defun make-random-point ()
  (make-point (rand-between *top* *bottom*)
	      (rand-between *left* *right*)))

(defun make-random-asteroid (position radius)
  (make-asteroid :start *frame*
		 :shape (make-asteroid-shape radius)
		 :radius radius
		 :velocity (make-point (rand-between (- *asteroid-max-speed*) *asteroid-max-speed*)
				       (rand-between (- *asteroid-max-speed*) *asteroid-max-speed*))
		 :origin position
		 :omega (rand-between *asteroid-min-omega* *asteroid-max-omega*)))

(defun action-duration (action-type)  
  (aif (get-action action-type)
       (- (aif (action-end it)
	       it
	       *frame*)
	  (action-start it))
       0))

(defun add-asteroid (position radius)
  (setf *asteroids* (cons (make-random-asteroid position radius) *asteroids*))
  (car *asteroids*))

(defun ship-angle ()
  (+ *ship-angle*
     (- (* (action-duration 'anticlockwise) *ship-omega*)
	(* (action-duration 'clockwise) *ship-omega*))))

(defun make-new-bullet ()
  (make-bullet :start *frame*
	       :angle (ship-angle)
	       :origin (translate-point (rotate-point *ship-nose* (ship-angle))
					(ship-position))))

(defun add-bullet () (setf *bullets* (cons (make-new-bullet) *bullets*)))

(defun make-ship () (velocity-transform (rotate-list *spaceship-shape* (ship-angle))
					*ship-position*
					*ship-position-reset*
					*ship-velocity*))

;;; key to action mapping- we could define the actions separately
;;; and give them a begin and end action function.

(defparameter *key-actions* '((#\z . anticlockwise)
			      (#\x . clockwise)
			      (#\m . fire)
			      (#\k . thrust)
			      (#\r . restart)))
  
;;; keys map to actions

(defun get-key-action-type (key)
  (aif (assoc key *key-actions*)
       (cdr it)))

(defun get-key-action (key)
  (aif (get-key-action-type key)
       (gethash it *actions*)))

(defun key-down (key)
  (aif (get-key-action-type key)
       (setf (gethash it *actions*)
		  (make-action :start *frame* :type it :end nil))))

(defun key-up (key)
  (aif (get-key-action key)  ;only interested if key-up was preceded by key-down
       (setf (action-end it) *frame*)))

(defun remove-action (action-type)
  (remhash action-type *actions*))

;;; open-gl stuff and main loop

(defun clear-asteroids () (setf *asteroids* nil))
(defun clear-bullets () (setf *bullets* nil))
(defun inc-frame () (setf *frame* (1+ *frame*)))

(defun get-action (action-type)
  (gethash action-type *actions*))

(defun action-begins (action) (= *frame* (action-start action)))
(defun action-ends (action) (aif (action-end action) (= *frame* it)))

;this may well depend on the action type e.g. ship asplodes
(defun should-reap-action (action)
  (aif (not (null (action-end action)))
    (>= *frame* (action-end action))))

;;;remove all actions which have timed out
(defun reap-actions ()
  (maphash #'(lambda (action-type action)
	       (aif (should-reap-action action)
		    (remhash action-type *actions*)))
	   *actions*))

(defun square (x) (* x x))

(defun magnitude (point)
  (sqrt (+ (square (point-x point))
	   (square (point-y point)))))

;; collision detection
;; we can avoid a square root here by doing d^2 < r1^2 + r2^2 + 2r1r2 

(defun collides (p1 r1 p2 r2)
  (< (magnitude (make-point (- (point-x p2) (point-x p1))
			    (- (point-y p2) (point-y p1))))
     (+ r1 r2)))

;;;getting the position of the bullet should be refactored to another function
;;;as it is used when drawing the shape too.
	
(defun bullet-asteroid-collides (bullet asteroid)
  (collides (bullet-position bullet) 0
	    (asteroid-position asteroid) (asteroid-radius asteroid)))

;;need to get some sort of idiomatic list filtering
;;that doesn't suck monkey balls.  But then again, does
;;state mutating in the forest make a sound if no-one is
;;there to hear it?
;;Additionally, we should pass through the calculated positions
;;into the mapc lambdas
(defun bullet-asteroid-collisions () 
  (let ((collisions nil))
    (mapc #'(lambda (bullet)
	      (mapc #'(lambda (asteroid)
			(if (and (not (asteroid-collided asteroid))
				 (bullet-asteroid-collides bullet asteroid))
			    (setf collisions (cons (list bullet asteroid) collisions))))
		    *asteroids*))
	  *bullets*)
    collisions))

(defun bullet-ufo-collisions ()
  (if *ufo-start*
      (remove-if-not #'(lambda (bullet)
			 (collides (bullet-position bullet) 0 (ufo-position) *ufo-radius*))
		     *bullets*)))

(defun bullet-ship-collisions ()
  (remove-if-not #'(lambda (bullet)
		     (collides (bullet-position bullet) 0 (ship-position) *ship-effective-radius*))
		 *bullets*))


(defun ship-asteroid-collision ()
  (let ((s (ship-position)))
    (find-if #'(lambda (asteroid) (and (not (asteroid-collided asteroid))
				       (collides s *ship-effective-radius*
						 (asteroid-position asteroid) (asteroid-radius asteroid))))
	     *asteroids*)))
			 
(defun maximize (point max)
  (let ((mag (magnitude point)))
    (if (<= mag max)
	point
	(scale point (/ max mag)))))

(defun apply-thrust ()
      (progn
	(setf *ship-position* (ship-position))
	(setf *ship-position-reset* *frame*)
	(setf *ship-velocity*
	      (maximize (translate-point *ship-velocity*
					 (rotate-point (make-point 0 *thrust*) (ship-angle)))
			*max-ship-speed*))))

(defun random-list-item (list)
  (nth (random (length list)) list))

(defun play-sample (sample)
  (if sample
      (sdl-mixer:play-sample sample)))

(defun play-explosion-sound ()
  (play-sample (random-list-item *explosion-sounds*)))

(defun play-ufo-ping-sound ()
  (play-sample (random-list-item *ufo-ping-sounds*)))

(defun play-level-music ()
  (play-music (nth (rem *level* (length *music-files*)) *music-files*)))

(defun play-shot-sound ()
  (play-sample *shot-sound*))

(defun start-game ()
  (setf *new-name* nil)
  (setf *lives* *starting-lives*)
  (setf *ufo-start* nil)
  (setf *frame* 0)
  (setf *asteroids* nil)
  (setf *bullets* nil)
  (setf *ship-position-reset* 0)
  (setf *ship-position* (make-point 0 0))
  (setf *ship-velocity* (make-point 0 0))
  (setf *actions* (make-hash-table))
  (setf *ship-angle* 0)
  (setf *score* 0)
  (setf *level* 0)
  (setf *next-wave* 0)
  (setf *life-start* 0))

(defun can-start ()
  (and (not (entering-hiscore)) (game-ended)))

;should use cond

(defun adjust-char (fn)
  (setf (elt *new-name* *new-name-cursor*) (funcall fn (elt *new-name* *new-name-cursor*))))

(defun insert-score (score name scores)
  (labels ((copy-rest (new rest index)
	     (if (= index *max-scores*)
		 (nreverse new)
		 (copy-rest (cons (car rest) new) (cdr rest) (1+ index))))
	   (copy-first (new rest index)
	     (if (= index *max-scores*)
		 scores
		 (if (> score (caar rest))
		     (copy-rest (cons (cons score name) new) rest (1+ index))
		     (copy-first (cons (car rest) new) (cdr rest) (1+ index))))))
    (copy-first nil scores 0)))

(defun load-scores ()
  (let ((score-file (open "hiscores" :if-does-not-exist nil)))
    (when score-file
      (setf *hi-scores* (read score-file))
      (close score-file))))

(defun save-scores ()
  (let ((score-file (open "hiscores" :direction :output :if-exists :supersede)))
    (when score-file
      (print *hi-scores* score-file)
      (close score-file))))

(defun finish-new-name ()
  (setf *hi-scores* (insert-score *score* *new-name* *hi-scores*))
  (save-scores)
  (setf *new-name* nil))

(defun game-ended ()
  (and (not (alive)) (> *frame* (+ *life-start* *post-game-delay*))))

(defun entering-hiscore ()
  (and *new-name* (game-ended)))

(defun process-actions ()
  (if (ship-ready)
					;clearly a dispatch on action type would be preferable
					;for a loopy solution
      (progn
	(aif (get-action 'fire)
	     (when (action-begins it)
	       (play-shot-sound)
	       (add-bullet)))
	;;solidify all that turning
	(aif (get-action 'clockwise)
	     (if (action-ends it)
		 (setf *ship-angle* (ship-angle))))
	(aif (get-action 'anticlockwise)
	     (if (action-ends it)
		 (setf *ship-angle* (ship-angle))))
	(if (get-action 'thrust)
	    (apply-thrust)) ;;this is the bit I don't like
	(reap-actions)))
  (if (can-start)
      (aif (get-action 'fire)
	   (if (action-begins it)
	       (start-game))))
  (when (entering-hiscore)
    (aif (get-action 'clockwise) (if (action-begins it) (adjust-char #'next-char)))
    (aif (get-action 'anticlockwise) (if (action-begins it) (adjust-char #'prev-char)))
    (aif (get-action 'fire) (if (action-begins it)
				(if (eq (incf *new-name-cursor*) 3)
				    (finish-new-name))))))

(defun clear-actions ()
  (setf *actions* (make-hash-table)))

(defun reap-bullets ()
  (setf *bullets* (delete-if #'(lambda (bullet)
				 (or (bullet-collided bullet)
				     (> (- *frame* (bullet-start bullet)) *bullet-duration*))) *bullets*)))

(defun reap-asteroids ()
  (setf *asteroids* (remove-if #'(lambda (asteroid)
				   (aif (asteroid-collided asteroid)
					(> (- *frame* it) 7)))
			       *asteroids*)))

(defun split-asteroid (asteroid)
  (play-explosion-sound)
  (setf (asteroid-collided asteroid) *frame*)
  (if (> (asteroid-radius asteroid) *asteroid-min-split-radius*)
      (progn
	(add-asteroid (asteroid-position asteroid) (* 0.5 (asteroid-radius asteroid)))
	(add-asteroid (asteroid-position asteroid) (* 0.5 (asteroid-radius asteroid))))))

(defun alive ()
  (> *lives* 0))

(defun ship-ready ()
  (and (alive) (>= *frame* *ship-position-reset*)))

(defun force-field ()
  (and (ship-ready) (or 
		    (<= *frame* (+ *force-field-delay* *life-start*))
		    (and (>= *frame* *next-wave*) (<= *frame* (+ *force-field-delay* *next-wave*))))))

(defun make-big-explosion (position)
  (dotimes (i 4) (setf (asteroid-collided (add-asteroid position (/ (1+ i) 15.0))) *frame*)))

(defun test-ufo-collision ()
  (aif (bullet-ufo-collisions)
       (progn
	 (decf *ufo-strength* (length it))
	 (mapc #'(lambda (bullet) (setf (bullet-collided bullet) *frame*)) it)
	 (play-ufo-ping-sound)
	 (incf *score* 1)
	 (when (zerop *ufo-strength*)
	   (make-big-explosion (ufo-position))
	   (incf *score* 10)
	   (setf *ufo-start* nil)))))

(defun kill-ship ()
  (make-big-explosion (ship-position))
  (setf *ship-position* (make-point 0 0))
  (setf *ship-position-reset* (+ *frame* *ship-delay*))
  (setf *ship-velocity* (make-point 0 0))
  (setf *ship-angle* 0)
  (setf *life-start* *frame*)
  (decf *lives*)
  (if (and (zerop *lives*) (is-hiscore))
      (setf *new-name* (make-string 3 :initial-element #\a))
      (setf *new-name-cursor* 0)))

(defun test-ship-collision ()
  (when (and (ship-ready) (not (force-field)))
    (aif (bullet-ship-collisions)
	 (progn
	 (mapc #'(lambda (bullet) (setf (bullet-collided bullet) *frame*)) it)
	 (kill-ship)))
    (if (and *ufo-start* (collides (ufo-position) *ufo-radius* (ship-position) *ship-effective-radius*))
	(kill-ship))
    (aif (ship-asteroid-collision)
	 (progn
	   (split-asteroid it)
	   (kill-ship)))))

(defun level-up ()
  (when (and (alive) (null *asteroids*) (> *frame* *next-wave*))
    (incf *level*)
    (play-level-music)
    (setf *next-wave* (+ *frame* *asteroid-create-delay*))))

(defun add-ufo ()
  (when (null *ufo-start*)
    (setf *ufo-strength* (* 2 *level*))
    (setf *ufo-position* (make-point *right* 0))
    (setf *ufo-start* *frame*)
    (setf *ufo-velocity* (make-point -0.005 -0.002))))

(defun ufo-fire ()
  (if (and (alive) *ufo-start* (<= (random 100) *level*))
	(setf *bullets* (cons (make-bullet :start (- *frame* 2) ;hack which is time sensitive!m
					   :origin (ufo-position)
					   :angle (/ *frame* 300)) *bullets*))))

(defun make-wave ()
  (let ((difficulty (floor *level* 2)))
    (progn
      (dotimes (i (+ difficulty 2)) (add-asteroid (make-random-point) *asteroid-max-radius*)))))


(defun update-scene ()
  (level-up)
  (if (and (alive) (= *frame* *next-wave*)) 
      (make-wave))
  (if (zerop (rem *frame* *ufo-period*))
      (add-ufo))
  (mapc #'(lambda (collision)
	    (destructuring-bind (bullet asteroid) collision
	      (setf *score* (+ *score* 1))
	      (setf (bullet-collided bullet) *frame*)
	      (split-asteroid asteroid)))
	(bullet-asteroid-collisions))
  (test-ship-collision)
  (test-ufo-collision)
  (ufo-fire)
  (reap-bullets)
  (reap-asteroids)
  (process-actions)
  (inc-frame))

(defun set-raster-colour () (gl:color 0.8 0.9 1.0))

(defun draw-shape (shape)
  (mapc #'(lambda (path)
	    (gl:with-primitive :line-strip
	    (set-raster-colour)
	    (mapc #'(lambda (point)
		      (gl:vertex (point-x point)
				 (point-y point))) path))) shape))

(defmacro restartable (&body body)
  `(restart-case
       (progn ,@body)
     (continue () :report "Continue" )))

(defun set-antialiasing ()   
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:enable :line-smooth)
  (gl:hint :line-smooth-hint :nicest))

(defun centre-word (word y)
  (multiple-value-bind (shape width) (make-word-shape word)
    (translate-list shape (- (/ width 2.0)) y)))

(defun is-hiscore ()
  (labels ((is (rest index)
	     (if (= index *max-scores*)
		 nil
		 (if (> *score* (caar rest))
		     index
		     (is (cdr rest) (1+ index))))))
    (is *hi-scores* 0)))

(defun draw-new-name ()
  (if (entering-hiscore)
      (draw-shape (centre-word *new-name* 0.7))))

(defun draw-hiscore (index score)
  (draw-shapes
   (let ((y (- *top* (* (+ index 5) *digit-height*)))) 
     (list
      (translate-list (make-number-shape (car score) *score-digits*)
		      (- (*  *score-digits* *digit-width*)) y)
      (translate-list (make-word-shape (cdr score))
		      (* 2 *digit-width*) y)))))

(defun next-char (char) (if (eq #\z char) #\a (code-char (1+ (char-code char)))))
(defun prev-char (char) (if (eq #\a char) #\z (code-char (1- (char-code char)))))

(defun draw-hiscores ()
  (labels ((draw (index rest)
	     (when (not (null rest))
	       (draw-hiscore index (car rest))
	       (draw (1+ index) (cdr rest)))))
    (draw 0 *hi-scores*)))
 
(defun draw ()
  (update-scene)
  (gl:clear :color-buffer-bit)
  (set-antialiasing)
  (draw-shapes (list (make-score) (make-lives)))
  (draw-shape (make-ufo))
  (draw-shapes (make-asteroids))
  (draw-shape (make-level))
  (when (ship-ready) ;this is in the wrong place, should pass in the list of shapes to draw so that this only has mapping logic from the shapes to the opengl
    (if (or (not (force-field)) (evenp *frame*))
	(draw-shape (make-ship)))
    (draw-shapes (make-bullets)))
  (draw-new-name)
  (if (not (alive)) (draw-shapes (list (centre-word "game over" 0.8))))
  (when (can-start)
	(draw-hiscores)
	(draw-shape (centre-word "Space Boulders" 0))
	(draw-shapes (scale-list (list (centre-word "Z LEFT K THRUST" (+ *bottom* 0.2))
				       (centre-word "X RIGHT M FIRE" (+ *bottom* 0.1)))
				 0.5)))
  (gl:flush)
  (sdl:update-display))

;;; convert sdl key mappings into nice sensible characters
;;; this function could be gold for all those poor googlers
(defparameter *sdl-key-mappings*
  (let ((table (make-hash-table)))
    (map nil #'(lambda (c)
		 (setf (gethash
			(find-symbol
			 (format nil "SDL-KEY-~a" (char-upcase c))
			 "KEYWORD")
			table) c))
	 "abcdefghijklmnopqrstuvwxyz01234567890")
    table))

(defun sdl-key-to-char (sdlkey)
  (gethash sdlkey *sdl-key-mappings*))

(defparameter *music* nil)

(defun play-music (filename)
  (when (not (null *music*))
    (sdl-mixer:Halt-Music)
    (sdl-mixer:free *music*))
  (setf *music* (sdl-mixer:load-music filename))
  (sdl-mixer:play-music *music* :loop t))

(defun init-sounds ()
  (sdl-mixer:init-mixer :mp3)
  (sdl-mixer:open-audio :chunksize 1024 :enable-callbacks nil)
  (sdl-mixer:allocate-channels 16)
  (play-level-music)
  (setf *shot-sound* (sdl-mixer:load-sample "sounds/shot.aif"))
  (setf *explosion-sounds* (list
			   (sdl-mixer:load-sample "sounds/explosion1.aif")
			   (sdl-mixer:load-sample "sounds/explosion2.aif")
			   (sdl-mixer:load-sample "sounds/explosion3.aif")
			   (sdl-mixer:load-sample "sounds/explosion4.aif")))
  (setf *ufo-ping-sounds* (list
			   (sdl-mixer:load-sample "sounds/ping1.aif")
			   (sdl-mixer:load-sample "sounds/ping2.aif")
			   (sdl-mixer:load-sample "sounds/ping3.aif")
			   (sdl-mixer:load-sample "sounds/ping4.aif"))))

(defun stop-music ()  (when (not (null *music*))
    (sdl-mixer:Halt-Music)
    (sdl-mixer:free *music*)
    (sdl-mixer:close-audio)
    (setf *music* nil)))

(defun main ()
  (unwind-protect ;not even sure this is necessary since it doesn't help at the repl
       (load-scores)
       (sdl:with-init ()
	 (init-sounds)
	 (sdl:disable-key-repeat)
	 (sdl:window 640 640 :flags sdl:sdl-opengl)
	 (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)
	 (sdl:with-events ()
	   (:key-down-event (:key key)
			    (if (sdl:key= key :sdl-key-escape)
				(sdl:push-quit-event)
				(aif (sdl-key-to-char key)
				     (key-down it))))
	   (:key-up-event (:key key)
			  (aif (sdl-key-to-char key)
			       (key-up it)))
	   (:quit-event ()
			  (stop-music)
			  t)
	   (:idle ()
		  #+(and sbcl (not sb-thread)) (restartable (sb-sys:serve-all-events 0))
		  (restartable (draw))))))
  (stop-music))
