;;
;; Exercise 2.84
;;
;; [Working]
;;

(load "numbers.scm")

;;
;; Import the procedures we defined in Exercise 2.78:
;;
(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
	((number? datum) 'scheme-number)
	(else
	 (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
	((number? datum) datum)
	(else
	 (error "Bad tagged datum -- CONTENTS" datum))))

;;
;; Bring over integer package from 2.83:
;;
(define (install-integer-package)
  (define (tag x)
    (cons 'integer x))
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'exp '(integer integer)
       (lambda (x y) (tag (expt x y))))
  (put 'equ? '(integer integer)
       (lambda (x y) (= x y)))
  (put '=zero? '(integer)
       (lambda (p) (= p 0)))
  (put 'make 'integer
       (lambda (x) (tag (if (integer? x) x (truncate x)))))
  'done)

(define (make-integer n)
  ((get 'make 'integer) n))

(define (add x y)
  (apply-generic 'add x y))
(define (sub x y)
  (apply-generic 'sub x y))
(define (mul x y)
  (apply-generic 'mul x y))
(define (div x y)
  (apply-generic 'div x y))
(define (exp x y)
  (apply-generic 'exp x y))
(define (equ? x y)
  (apply-generic 'equ? x y))
(define (=zero? p)
  (apply-generic '=zero? p))

(install-integer-package)

;;
;; Onto the answer for the problem:
;;

;;
;; As example of what we are trying to do, is iteratively applying 'apply':
;;
(raise (make-integer 1))
;; ==> (rational 1 . 1)
(raise (raise (make-integer 1)))
;; ==> 1.
(raise (raise (raise (make-integer 1))))
;; ==> (complex rectangular 1. . 0)

;;
;; [WORKING] (we need a way to compare the hierarchy).
;; (new levels can be added to hierarchy by adjusting the numbers).

(define (type-level type)
  (cond ((eq? type 'integer) 0)
	((eq? type 'rational) 1)
	((eq? type 'scheme-number) 2)
	((eq? type 'complex) 3)
	(else
	 (error "Invalid type: LEVEL" type))))

;;
;; Unit tests:
;;
(type-level 'integer)
;; ==> 0
(type-level 'ratioanl)
;; ==> 1
(type-level 'scheme-number)
;; ==> 2
(type-level 'complex)
;; ==> 3

(define (highest-type-level args)
  (if (null? args)
      0
      (let ((level (type-level (car args)))
	    (highest (highest-type-level (cdr args))))
	(if (> level highest)
	    level
	    highest))))
(highest-type-level '(integer integer))


