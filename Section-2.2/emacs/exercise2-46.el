;;
;; Exercise 2.46
;;
;; A two-dimensional vector v running from the origin to a point can be represented as a pair consisting
;; of an x-coordinate and a y-coordinate. Implement a data abstraction for vectors by giving a constructor
;; "make-vect" and corresponding selectors "xcor-vect" and "ycor-vect". In terms of your selectors and constructor,
;; implement procedures "add-vect", "sub-vect" and "scale-vect" that perform the operations vector addition,
;; vector subtraction and multiplying a vector by a scalar.
;;

;;
;; Note: we will use the same data model that is used in "SICP Picture Language" for Dr. Racket
;;

;;
;; Constructors and Selectors:
;;
(defun make-vect (x y)
  (cons x y))

(defun xcor-vect (p)
  (car p))

(defun ycor-vect (p)
  (cdr p))

;;
;; Mathematical operations:
;;
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
;; Run some unit tests:
;;
(setq v (make-vect 1 2))
;; ==> (1 . 2)
(xcor-vect v)
;; ==> 1
(ycor-vect v)
;; ==> 2

(setq u (make-vect 3 4))
;; ==> (3 . 4)

;;
;; Test out the mathematical operations:
;;
(add-vect u v)
;; ==> (4 . 6)
(xcor-vect (add-vect u v))
;; ==> 4
(ycor-vect (add-vect u v))
;; ==> 6

(sub-vect u v)
;; ==> (2 . 2)

(scale-vect 3 u)
;; ==> (9 . 12)
