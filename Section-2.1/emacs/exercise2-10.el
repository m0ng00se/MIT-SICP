;;
;; Exercise 2.10
;;
;; Ben Bitdiddle, an expert systems programmer, looks over Alyssa's shoulder and comments
;; that it is not clear what it means to divide by an interval that spans zero. Modify
;; Alyssa's code to check for this condition and to signal an error if it occurs.
;;

;;
;; Define supporting procedures:
;;
(defun make-interval (a b) (cons a b))
(defun lower-bound (x) (car x))
(defun upper-bound (x) (cdr x))

(defun add-interval (x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(defun mul-interval (x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

;;
;; Original definition of "div-interval" given by Alyssa:
;;
(defun div-interval (x y)
  (mul-interval x
		(make-interval (/ 1.0 (upper-bound y))
			       (/ 1.0 (lower-bound y)))))

;;
;; Modified version that checks for division by zero:
;;
;; Since we use the forms (/ 1.0 (upper-bound y)) and (/ 1.0 (lower-bound y))
;; in forming the result for "div-interval", and since we cannot divide by zero,
;; we will signal an error if either (lower-bound y) or (upper-bound y) is zero.
;;
;; More generally, we can model any interval that "spans" 0 as somehow being a 
;; representation "of" 0, and hence exclude such candidate intervals from division.
;; Note that the previous checks we describe are a special case of this more 
;; general check.
;;
(defun div-interval (x y)
  (let ((p1 (lower-bound y))
	(p2 (upper-bound y)))
    (defun spans-zero? ()
      (and (< p1 0) (> p2 0)))
    (cond ((or (= p1 0) (= p2 0) (spans-zero?))
	   (princ "*** Cannot divide by zero"))
	  (t
	   (mul-interval x
			 (make-interval (/ 1.0 (upper-bound y))
					(/ 1.0 (lower-bound y))))))))

;;
;; Run some unit tests:
;;
(setq i1 (make-interval 0 2))
;; ==> (0 . 2)
(setq i2 (make-interval -2 0))
;; ==> (-2 . 0)
(setq i3 (make-interval -2 -2))
;; ==> (-2 . 2)
(setq i4 (make-interval 5 7))
;; ==> (5 . 7)

;;
;; These should result in something approximating zero:
;;
(div-interval i1 i4)
;; ==> (0. . .4)
(div-interval i2 i4)
;; ==> (-.4 . 0.)
(div-interval i3 i4)
;; ==> (-.4 . .4)

;;
;; These should trigger divide-by-zero errors:
;;
(div-interval i4 i1)
;; ==> *** Cannot divide by zero!
(div-interval i4 i2)
;; ==> *** Cannot divide by zero!
(div-interval i4 i3) 
;; ==> *** Cannot divide by zero!

;;
;; This should result in something approximating one:
;;
(div-interval i4 i4)
;; ==> (0.714285... . 1.4)
