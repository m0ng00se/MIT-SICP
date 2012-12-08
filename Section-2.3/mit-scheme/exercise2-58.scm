;;
;; 

;;
;; Again, import the procedures from the working differentiator in Exercise 2.56.
;;
;; These are all defined for "binary" operations, not the arbitrary length sums
;; and products as we defined in Exercise 2.57:
;;

;;
;; Procedures for object identification:
;;
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num)
  (and (number? exp) (= exp num)))

;;
;; Procedures for handling sums:
;;
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else
	  (list '+ a1 a2))))

;;
;; Procedures for handling products:
;;
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else
	  (list '* m1 m2))))

;;
;; Procedures for handling differences:
;;
(define (difference? x)
  (and (pair? x) (eq? (car x) '-)))
(define (minuend p) (cadr p))
(define (subtrahend p) (caddr p))
(define (make-difference s1 s2)
  (cond ((=number? s2 0) s1)
	((=number? s1 0) (make-product -1 s2))
	((and (number? s1) (number? s2)) (- s1 s2))
	(else
	  (list '- s1 s2))))

;;
;; Procedures for handling exponentiation:
;;
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**) (variable? (cadr x))))
(define (base p) (cadr p))
(define (exponent p) (caddr p))
(define (make-exponentiation base exp-value)
  (cond ((and (number? base) (number? exp-value)) (expt base exp-value))
	((=number? exp-value 0) 1)
	((=number? exp-value 1) base)
	(else
	  (list '** base exp-value))))

;;
;; Define the "deriv" procedure:
;;
(define (deriv expression var)

  (cond 
   ((number? expression) 0) 
   
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
   
   (else
    (error "Unknown expression type -- DERIV" expression))))

;;
;; In principle, so long as the combination operations remain "binary" (i.e., two 
;; arguments), we should be able to implement this change by just changing where 
;; in the list the combination symbol shows up. So, for instance, (+ 1 2) becomes 
;; (1 + 2), and (* 1 2) becomes (1 * 2), and so forth.
;;

;;
;; Let's see if we can implement (a) just by changing a few constructors/selectors:
;;

;;
;; Summations:
;;
(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))
(define (addend s)
  (car s))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else
	 (list a1 '+ a2))))

(make-sum 0 10)
;; ==> 10
(make-sum 0 'x)
;; ==> x
(make-sum 10 0)
;; ==> 10
(make-sum 'x 0)
;; ==> x
(make-sum 10 20)
;; ==> 30
(make-sum 'x 'y)
;; ==> (x + y)
(make-sum 10 'x)
;; ==> (10 + x)
(make-sum 'x 10)
;; ==> (x + 10)

(define v1 (make-sum 'x 'y))
(sum? v1)
;; ==> #t
(addend v1)
;; ==> x
(augend v1)
;; ==> y

;;
;; Products:
;;
(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))
(define (multiplier p) (car p))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number?  m2)) (* m1 m2))
	(else 
	  (list m1 '* m2))))

(make-product 0 10)
;; ==> 0
(make-product 1 10)
;; ==> 10
(make-product 2 10)
;; ==> 20
(make-product 10 0)
;; ==> 0
(make-product 10 1)
;; ==> 10
(make-product 10 2)
;; ==> 20
(make-product 0 'x)
;; ==> 0
(make-product 1 'x)
;; ==> x
(make-product 2 'x)
;; ==> (2 * x)
(make-product 'x 0)
;; ==> 0
(make-product 'x 1)
;; ==> 1
(make-product 'x 2)
;; ==> (x * 2)
(make-product 'x 'y)
;; ==> (x * y)

(define v2 (make-product 'x 'y))
(product? v2)
;; ==> #t
(multiplier v2)
;; ==> x
(multiplicand v2)
;; ==> y

;;
;; Differences:
;;
(define (difference? x)
  (and (pair? x) (eq? (cadr x) '-)))
(define (minuend p) (car p))
(define (make-difference s1 s2)
  (cond ((=number? s2 0) s1)
	((=number? s1 0) (make-product -1 s2))
	((and (number? s1) (number? s2)) (- s1 s2))
	(else
	 (list s1 '- s2))))

(make-difference 3 0)
;; ==> 3
(make-difference 0 3)
;; ==> -3 
(make-difference 4 2)
;; ==> 2
(make-difference 2 4)
;; ==> -2
(make-difference 'x 0)
;; ==> x
(make-difference 0 'x)
;; ==> (-1 * x)
(make-difference 'x 1)
;; ==> (x - 1)
(make-difference 1 'x)
;; ==> (1 - x)
(make-difference 'x 'y)
;; ==> (x - y)

(define v3 (make-difference 'x 'y))
(difference? v3)
;; ==> #t
(minuend v2)
;; ==> x
(subtrahend v2)
;; ==> y

;;
;; Exponentiation:
;;
(define (exponentiation? x)
  (and (pair? x) (eq? (cadr x) '**) (variable? (car x))))
(define (base p)
  (car p))
(define (make-exponentiation base exp-value)
  (cond ((and (number? base) (number? exp-value)) (expt base exp-value))
	((=number? exp-value 0) 1)
	((=number? exp-value 1) base)
	(else
	 (list base '** exp-value))))

(make-exponentiation 3 0)
;; ==> 1
(make-exponentiation 3 1)
;; ==> 3
(make-exponentiation 3 2)
;; ==> 9
(make-exponentiation 'x 0)
;; ==> 1
(make-exponentiation 'x 1)
;; ==> x
(make-exponentiation 'x 2)
;; ==> (x ** 2)
(make-exponentiation 3 'x)
;; ==> (3 ** x)
(make-exponentiation 'x 'x)
;; ==> (x ** x)

;;
;; That should be all we have to change.
;;
;; Let's walk through some use cases, to see if the differentiator gives the correct answer:
;;
(deriv 3 'x)
;; ==> 0
(deriv 'x 'x)
;; ==> 1
(deriv 'x 'y)
;; ==> 0
(deriv '(x + y) 'x)
;; ==> 1
(deriv '(x + y) 'y)
;; ==> 1
(deriv '(x + y) 'z)
;; ==> 0
(deriv '((2 * x) + y) 'x)
;; ==> 2
(deriv '((2 * x) + y) 'y)
;; ==> 1
(deriv '((x * y) + y) 'x)
;; ==> y
(deriv '((x * y) + y) 'y)
;; ==> (x + 1)
(deriv '(x - 1) 'x)
;; ==> 1
(deriv '(y - x) 'x)
;; ==> -1
(deriv '(x * y) 'x)
;; ==> y 
(deriv '(x ** 3) 'x)
;; ==> (3 * (x ** 2))
(deriv '(x ** y) 'x)
;; ==> (y * (x ** (y - 1)))


(deriv '(x + 3) 'x)
;; ==> 1
(deriv '(x * y) 'x)
;; ==> y 
(deriv '(x * x) 'x)
;; ==> (x + x)
(deriv '((x * y) + (x + 3)) 'x)
;; ==> (y + 1)
(deriv '((x * y) * (x + 3)) 'x)
;; ==> ((x * y) + (y * (x + 3)))

;;
;; Indeed, the procedeure seems to work correctly.
;;