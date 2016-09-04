;;
;; Procedures for performing symbolic differentiation:
;;

;;
;; Procedures for object identification:
;;
(defun variable? (x) (symbolp x))
(defun same-variable? (v1 v2)
  (and (variable? v1) (variable? v2) (eq v1 v2)))
(defun =number? (exp num)
  (and (numberp exp) (= exp num)))

;;
;; Procedures for handling sums:
;;
(defun sum? (x)
  (and (listp x) (eq (car x) '+)))
(defun addend (s) (cadr s))
(defun augend (s) (car (cdr (cdr s))))
(defun make-sum (a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (numberp a1) (numberp a2)) (+ a1 a2))
	(t
	 (list '+ a1 a2))))

;;
;; Procedures for handling products:
;;
(defun product? (x)
  (and (listp x) (eq (car x) '*)))
(defun multiplier (p) (cadr p))
(defun multiplicand (p) (car (cdr (cdr p))))
(defun make-product (m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (numberp m1) (numberp m2)) (* m1 m2))
	(t
	 (list '* m1 m2))))

;;
;; Procedures for handling differences:
;;
(defun difference? (x)
  (and (listp x) (eq (car x) '-)))
(defun minuend (p) (cadr p))
(defun subtrahend (p) (car (cdr (cdr p))))
(defun make-difference (s1 s2)
  (cond ((=number? s2 0) s1)
	((=number? s1 0) (make-product -1 s2))
	((and (numberp s1) (numberp s2)) (- s1 s2))
	(t
	 (list '- s1 s2))))

;;
;; Procedures for handling exponentiation:
;;
(defun exponentiation? (x)
  (and (listp x) (eq (car x) '**) (variable? (cadr x))))
(defun base (p)
  (cadr p))
(defun exponent (p)
  (car (cdr (cdr p))))
(defun make-exponentiation (base exp-value)
  (cond ((and (numberp base) (numberp exp-value)) (expt base exp-value))
	((=number? exp-value 0) 1)
	((=number? exp-value 1) base)
	(t
	 (list '** base exp-value))))

;;
;; Define the "deriv" procedure:
;;
(defun deriv (expression var)
  (cond ((numberp expression) 0)
	((variable? expression)
	 (if (same-variable? expression var) 1 0))
	((sum? expression)
	 (make-sum (deriv (addend expression) var)
		   (deriv (augend expression) var)))
	((difference? expression)
	 (make-difference (deriv (minuend expression) var)
			  (deriv (subtrahend expression) var)))
	((product? expression)
	 (make-sum
	  (make-product (multiplier expression)
			(deriv (multiplicand expression) var))
	  (make-product (deriv (multiplier expression) var)
			(multiplicand expression))))
	((exponentiation? expression)
	 (make-product
	  (exponent expression)
	  (make-exponentiation
	   (base expression)
	   (make-difference (exponent expression) 1))))
	(t
	 (error (princ "Unknown expression type -- DERIV") 1))))
