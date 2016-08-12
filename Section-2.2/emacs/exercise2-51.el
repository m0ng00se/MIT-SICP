;;
;; Exercise 2.51
;;
;; Define the "below" operation for painters. "Below" takes two painters as arguments. The resulting
;; painter, given a frame, draws with the first painter in the bottom of the frame and wiht the
;; second painter in the top. Define "below" in two different ways -- first by writing a procedure
;; that is analogous to the "beside" procedure given above, and again in terms of "beside" and
;; suitable rotation operations (see Exercise 2.50)
;;

;; Load the procedures from previous exercise:
(load-file "exercise2-50.el")

;;
;; Using the first approach:
;;
(defun below (painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
	   (transform-painter painter1
			      (make-vect 0.0 0.0)
			      (make-vect 1.0 0.0)
			      split-point))
	  (paint-top
	   (transform-painter painter2
			      split-point
			      (make-vect 1.0 0.5)
			      (make-vect 0.0 1.0))))
      (lambda (frame)
	(paint-bottom frame)
	(paint-top frame)))))

;;
;; Sanity check:
;;
(below square square)
(below square diamond)
(below square wave)

;;
;; Using the second approach:
;;
(defun below (painter1 painter2)
  (rotate270 (beside (rotate90 painter2) (rotate90 painter1))))

;;
;; Sanity check:
;;
(below square square)
(below square diamond)
(below square wave)
