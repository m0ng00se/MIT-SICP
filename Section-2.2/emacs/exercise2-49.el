;;
;; Exercise 2.49
;;
;; Use "segments->painter" to define the following primitive painters:
;;
;;  (a) The painter that draws the outline of the designated frame.
;;  (b) The painter that draws an "X" by connecting opposite corners of the frame.
;;  (c) The painter that draws a diamond shape by connecting the midpoints of the sides of the frame.
;;  (d) The "wave" painter.
;;

;;
;; The supporting vector procedures are given here:
;;
(defun make-vect (x y)
  (cons x y))
(defun xcor-vect (p)
  (car p))
(defun ycor-vect (p)
  (cdr p))

(defun add-vect (a b)
  (make-vect
   (+ (xcor-vect a) (xcor-vect b))
   (+ (ycor-vect a) (ycor-vect b))))
(defun sub-vect (a b)
  (make-vect
   (- (xcor-vect a) (xcor-vect b))
   (- (ycor-vect a) (ycor-vect b))))
(defun scale-vect (c a)
  (make-vect
   (* c (xcor-vect a))
   (* c (ycor-vect a))))

(defun make-frame (origin edge1 edge2)
  (list 'frame origin edge1 edge2))
(defun origin-frame (f)
  (car (cdr f)))
(defun edge1-frame (f)
  (car (cdr (cdr f))))
(defun edge2-frame (f)
  (car (cdr (cdr (cdr f)))))

(defun make-segment (start end)
  (cons start end))
(defun start-segment (segment)
  (car segment))
(defun end-segment (segment)
  (cdr segment))

;;
;; The "frame-coord-map" procedure is given as:
;;
(defun frame-coord-map (frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
			   (edge1-frame frame))
	       (scale-vect (ycor-vect v)
			   (edge2-frame frame))))))

;;
;; The "segments->painter" procedure is given as:
;;
(defun segments->painter (segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
	((frame-coord-map frame) (start-segment segment))
	((frame-coord-map frame) (end-segment segment))))
     segment-list)))

;;
;; As indicated in the text, these procedures are designed to draw inside
;; the unit square. If we are using Dr. Scheme to render the images, we need
;; to substract a small "epsilon" from the edge to make sure it renders
;; directly onto the screen, e.g., (define p2 (make-vect 0.99 0)).
;;
;; For the purposes of this discussio, we will simply use the more natural
;; number representation:
;;
(setq p1 (make-vect 0 0))
(setq p2 (make-vect 1 0))
(setq p3 (make-vect 1 1))
(setq p4 (make-vect 0 1))

;; We'll draw the "diamond" by using our vector composition procedures:
(setq p5 (scale-vect 0.5 (add-vect p1 p2)))
(setq p6 (scale-vect 0.5 (add-vect p2 p3)))
(setq p7 (scale-vect 0.5 (add-vect p3 p4)))
(setq p8 (scale-vect 0.5 (add-vect p4 p1)))

(setq s1 (make-segment p1 p2))
(setq s2 (make-segment p2 p3))
(setq s3 (make-segment p3 p4))
(setq s4 (make-segment p4 p1))

(setq s5 (make-segment p1 p3))
(setq s6 (make-segment p2 p4))

(setq s7 (make-segment p5 p6))
(setq s8 (make-segment p6 p7))
(setq s9 (make-segment p7 p8))
(setq s10 (make-segment p5 p5))

;; draw the "wave" man:
(setq points
      '((0.4 0.0)
	(0.5 0.33)
	(0.6 0.0)
	(0.75 0.0)
	(0.6 0.45)
	(0.99 0.15)
	(0.99 0.35)
	(0.8 0.65)
	(0.6 0.65)
	(0.65 0.8)
	(0.6 0.99)
	(0.4 0.99)
	(0.35 0.8)
	(0.4 0.65)
	(0.33 0.65)
	(0.1 0.6)
	(0.0 0.8)
	(0.0 0.6)
	(0.1 0.4)
	(0.3 0.6)
	(0.33 0.5)
	(0.25 0.0)))

;; map the "make-vect" procedure over these points:
(defun make-vectors (points)
  (mapcar (lambda (p)
	    (let ((x (car p))
		  (y (cadr p)))
	      (make-vect x y)))
	  points))

(setq vectors (make-vectors points))

;; create a way to generate segments from these vectors:
(defun make-segments (vectors)
  (defun make-segments-iter (working total)
    (if (null (cdr working))
	(append total (list (make-segment (car working) (car (car total)))))
      (let ((one (car working))
	    (two (cadr working)))
	(make-segments-iter (cdr working) (append total (list (make-segment one two)))))))
  (make-segments-iter vectors '()))

(setq segments (make-segments vectors))

;; (a) The painter that draws the outline of the designated frame:
(setq square (segments->painter (list s1 s2 s3 s4)))

;; (b) The painter that draws an "X" by connecting opposite corners of the frame:
(setq x-marks-the-spot (segments->painter (list s5 s6)))

;; (c) The painter that draws a diamond shape by connecting the midpoints of the sides of the frame:
(setq diamond (segments->painter (list s7 s8 s9 s10)))

;; (d) The "wave" painter:
(setq wave (segments->painter segments))

;;
;; See the attached .md file for actual pictures.
;;
