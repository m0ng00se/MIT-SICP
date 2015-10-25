;;
;; [Exercise 3.30]
;;
;; [Working]
;;

;;
;; Import the circuit library:
;;
(load "circuit.scm")

;;
;; Import the or-gate from Exercise 3.28, and the and-gate and 
;; inverter from Exercise 3.29:
;;
(define (logical-or i1 i2)
  (define (error-handler)
    (error "Invalid signal" i1 i2))
  (if (and (or (= i1 0) (= i1 1))
	   (or (= i2 0) (= i2 1)))
      (cond ((or  (= i1 1) (= i2 1)) 1)
	    ((and (= i1 0) (= i2 0)) 0)
	    (else
	     (error-handler)))
      (error-handler)))

(define (or-gate i1 i2 output)
  (define (or-action-procedure)
    (let ((new-value 
	   (logical-or (get-signal i1) (get-signal i2))))
      (after-delay or-gate-delay
		   (lambda ()
		     (set-signal! output new-value)))))
  (add-action! i1 or-action-procedure)
  (add-action! i2 or-action-procedure)
  'ok)

(define (logical-and i1 i2)
  (define (error-handler)
    (error "Invalid signal" i1 i2))
  (if (and (or (= i1 0) (= i1 1))
	   (or (= i2 0) (= i2 1)))
      (cond ((and (= i1 1) (= i2 1)) 1)
	    ((or (= i1 0) (= i2 0)) 0)
	    (else
	     (error-handler)))
      (error-handler)))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
	   (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
		   (lambda ()
		     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
	((= s 1) 0)
	(else
	 (error "Invalid signal" s))))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
		   (lambda ()
		     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

;;
;; First we must define and half-adder and full-adder procedures:
;;
(define (half-adder a b s c)
  (let ((d (make-wire))
	(e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
	(c1 (make-wire))
	(c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

;;
;; The logic table for a half-adder looks like the following:
;;
;; +---+---+---+---+---+---+
;; | A | B | D | E | S | C |
;; +---+---+---+---+---+---+
;; | 1 | 1 | 1 | 0 | 0 | 1 |
;; | 1 | 0 | 1 | 1 | 1 | 0 |
;; | 0 | 1 | 1 | 1 | 1 | 0 |
;; | 0 | 0 | 0 | 1 | 0 | 0 |
;; +---+---+---+---+---+---+
;;
;; S is 1 whenever precisely one of A and B is 1 (exclusive-or), and C is 1 
;; whenever both A and B are 1 (logical-and). 
;;
;; The half-adder defines a mechanism for adding two bits, A and B, 
;; to obtain a 2-bit number as specified by the sum and carry bits:
;;
;;  1 + 1 ==> 10
;;  1 + 0 ==> 01
;;  0 + 1 ==> 01
;;  0 + 0 ==> 00 
;;
;; The logic table for a full-adder looks like the following:
;;
;; +---+---+-------+----+----+---+-----+--------+
;; | A | B | C(in) | C1 | C2 | S | Sum | C(out) |
;; +---+---+-------+----+----+---+-----+--------+
;; | 1 | 1 |   1   |  1 |  0 | 0 |  1  |   1    |
;; | 1 | 1 |   0   |  0 |  1 | 1 |  0  |   1    |
;; | 1 | 0 |   1   |  0 |  1 | 1 |  0  |   1    |
;; | 1 | 0 |   0   |  0 |  0 | 0 |  1  |   0    |
;; | 0 | 1 |   1   |  1 |  0 | 0 |  0  |   1    |
;; | 0 | 1 |   0   |  0 |  0 | 1 |  1  |   0    |
;; | 0 | 0 |   1   |  0 |  0 | 1 |  1  |   0    |
;; | 0 | 0 |   0   |  0 |  0 | 0 |  0  |   0    |
;; +---+---+-------+----+----+---+-----+--------+
;;
;; The full-adder defines a mechanism for adding two bits, A and B, 
;; together with a carry bit, to obtain a 2-bit number as specified
;; by the sum and carry-out bits:
;;
;;  1 + 1 + (1) ==> 11
;;  1 + 1 + (0) ==> 10
;;  1 + 0 + (1) ==> 10
;;  1 + 0 + (0) ==> 01
;;  0 + 1 + (1) ==> 10 
;;  0 + 1 + (0) ==> 01
;;  0 + 0 + (1) ==> 01
;;  0 + 0 + (0) ==> 00
;; 

(define (ripple-carry-adder a-list b-list s-list c)
  (define (ripple-carry-adder-iter a b c-in s)
    (let ((a1 (car a))
	  (b1 (car b))
	  (s1 (car s)))
      (if (and (not (null? a1))
	       (not (null? b1))
	       (not (null? s1)))
	  (let ((c-out (make-wire)))
	    (let ((fa (full-adder a1 b1 c-in s1 c-out)))
	      (let ((an (cdr a))
		    (bn (cdr b))
		    (sn (cdr s)))
		(if (and (not (null? an))
			 (not (null? bn))
			 (not (null? sn)))
		    (ripple-carry-adder-iter an bn c-out sn))))))))

  (ripple-carry-adder-iter a-list b-list c s-list))
  

(define (ripple-carry-adder A B S c)
  (let ((a1 (car A))
	(b1 (car B))
	(s1 (car S)))
    (if (and (not (null? a1))
	     (not (null? b1))
	     (not (null? c1)))
	(let ((fa (full-adder a1 b1 c s1 c-out)
	'()
	'())))
