;;
;; Exercise 2.48
;;
;; A directed line segment in a plane can be represented as a pair of vectors -- the vector running from the
;; origin to the start-point of the segment, and the vector running from the origin to the end-point of the
;; segment. Use your vector representation from Exercise 2.46 to define a representation for segments with
;; a constructor "make-segment" and selectors "start-segment" and "end-segment".
;;

;;
;; First import the necessary code from Exercise 2.46:
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

;;
;; Note: we will use the same data model that is used in "SICP Picture Language" for Dr. Racket
;;

;;
;; Now define the constructors and selectors:
;;
(defun make-segment (start end)
  (cons start end))
(defun start-segment (segment)
  (car segment))
(defun end-segment (segment)
  (cdr segment))

;;
;; Run some unit tests:
;;
(setq p1 (make-vect 1 2))
;; ==> (1 . 2)
(setq p2 (make-vect 3 5))
;; ==> (3 . 5)

(setq s (make-segment p1 p2))
;; ==> ((1 . 2) 3 . 5)
(start-segment s)
;; ==> (1 . 2)
(end-segment s)
;; ==> (3 . 5)
