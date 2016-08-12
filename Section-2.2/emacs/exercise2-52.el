
;; Load procedures from previous exercises:
(load-file "exercise2-51.el")

;; now define the "smile":
(setq p1 (make-vect 0.45 0.75))
(setq p2 (make-vect 0.5 0.7))
(setq p3 (make-vect 0.55 0.75))

(setq s1 (make-segment p1 p2))
(setq s2 (make-segment p2 p3))

(setq segments (append segments (list s1)))
(setq segments (append segments (list s2)))

(setq wave (segments->painter segments))

;; (b) Change the pattern constructed by "corner-split" (for example, by using only one copy of the "up-split" and "right-split" images instead of two)

;;
;; The definitions of "right-split" and "up-split" are given as:
;;
(defun right-split (painter n)
  (if (= n 0)
      painter
    (let ((smaller (right-split painter (- n 1))))
      (beside painter (below smaller smaller)))))

(defun up-split (painter n)
  (if (= n 0)
      painter
    (let ((smaller (up-split painter (- n 1))))
      (below painter (beside smaller smaller)))))

;;
;; Sanity check:
;;
(right-split square 3)
(up-split square 3)

;;
;; This is the definition of "corner-split" given in the text:
;;
(defun corner-split (painter n)
  (if (= n 0)
      painter
    (let ((up (up-split painter (- n 1)))
	  (right (right-split painter (- n 1))))
      (let ((top-left (beside up up))
	    (bottom-right (below right right))
	    (corner (corner-split painter (- n 1))))
	(beside (below painter top-left)
		(below bottom-right corner))))))

;;
;; We can change the definition of "corner-split" as suggested in the text:
;;
(defun corner-split (painter n)
  (if (= n 0)
      painter
    (let ((up (up-split painter (- n 1)))
	  (right (right-split painter (- n 1))))
      (let ((top-left up)
	    (bottom-right right)
	    (corner (corner-split painter (- n 1))))
	(beside (below painter top-left)
		(below bottom-right corner))))))

;; (c) Modify the version of "square-limit" that uses "square-of-four" so as to assemble the corners in a different pattern (for example, you might make the big Mr. Rogers look outward from each corner of the square).

;;
;; The definition of "square-limit" (using "square-of-four") as given in the text is as follows:
;;
(defun identity (x) x)

(defun square-of-four (tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
	  (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(defun square-limit (painter n)
  (let ((combine4 (square-of-four flip-horiz identity rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

;;
;; To reverse the way that Einstein "looks" in the picture, we reveres the order in which
;; the "square-of-four" procedures are applied:
;;
(defun square-limit (painter n)
  (let ((combine4 (square-of-four flip-vert rotate180 identity flip-horiz)))
    (combine4 (corner-split painter n))))

;;
;; Sanity check:
;;
(corner-split square 3)
(identity square)
(square-of-four square square square square)
(square-limit square 3)

;;
;; Pictures for all these exercises are given in the accompanying .md file.
;;
       
