;;
;; Exercise 2.50
;;
;; Define the transformation "flip-horiz" which flips painters horizontally, and transformations
;; that rotate painters counterclockwise by 180 degrees and 270 degrees.
;;

;; Load previous procedures:
(load-file "exercise2-49.el")

;;
;; The "transform-painter" procedure from the text:
;;
(defun transform-painter (painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
	(painter
	 (make-frame new-origin
		     (sub-vect (m corner1) new-origin)
		     (sub-vect (m corner2) new-origin)))))))

(defun beside (painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
	   (transform-painter painter1
			      (make-vect 0.0 0.0)
			      split-point
			      (make-vect 0.0 1.0)))
	  (paint-right
	   (transform-painter painter2
			      split-point
			      (make-vect 1.0 0.0)
			      (make-vect 0.5 1.0))))
      (lambda (frame)
	(paint-left frame)
	(paint-right frame)))))

;;
;; Rotate90 procedure from the text:
;;
(defun rotate90 (painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)))

;;
;; flip-vert procedure from the text:
;;
(defun flip-vert (painter)
  (transform-painter painter
		     (make-vect 0.0 1.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)))

;;
;; Answers to the questions:
;;
(defun flip-horiz (painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)   ;; new origin
		     (make-vect 0.0 0.0)   ;; new end of edge 1
		     (make-vect 1.0 1.0))) ;; new end of edge 2

(defun rotate180 (painter)
  (transform-painter painter
		     (make-vect 1.0 1.0)   ;; new origin
		     (make-vect 0.0 1.0)   ;; new end of edge 1
		     (make-vect 1.0 0.0))) ;; new end of edge 2

(defun rotate270 (painter)
  (transform-painter painter
		     (make-vect 0.0 1.0)   ;; new origin
		     (make-vect 0.0 0.0)   ;; new end of edge 1
		     (make-vect 1.0 1.0))) ;; new end of edge 2

;;
;; As a sanity check, make sure that the painters defined in exercise2-49 all "compile":
;;
(flip-horiz square)
(flip-horiz x-marks-the-spot)
(flip-horiz diamond)
(flip-horiz wave)

(rotate180 square)
(rotate180 x-marks-the-spot)
(rotate180 diamond)
(rotate180 wave)

(rotate270 square)
(rotate270 x-marks-the-spot)
(rotate270 diamond)
(rotate270 wave)
