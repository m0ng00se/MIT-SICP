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
;; Import the or-gate from Exercise 3.28:
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

;;
;; Import the and-gate from Exercise 3.29:
;;
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

;;
;; Import the inverter from Exercise 3.29:
;;
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
;; We define the half-adder procedure:
;;
(define (half-adder a b s c)
  (let ((d (make-wire))
	(e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
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

;; 
;; Run some unit tests using the half-adder:
;;
(define a (make-wire))
(define b (make-wire))
(define s (make-wire))
(define c (make-wire))

(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(probe 's s)
;; ==> 0 New-value = 0
(probe 'c c)
;; ==> 0 New-value = 0

(half-adder a b s c)
;; ==> ok

(get-signal a)
;; ==> 0
(get-signal b)
;; ==> 0
(get-signal s)
;; ==> 0
(get-signal c)
;; ==> 0

(set-signal! a 1)
(propagate)
;; ==> s 8 New-value 1

;;
;; Setting the signal on A to 1 causes the signal at S to go to 1 as well,
;; after a delay of 8 time units. A time-analysis of how signals propagate 
;; within the half-adder is as follows:
;;
;;  Signal D: changes after 1 or-gate-delay, or 5 units;
;;  Signal C: changes after 1 and-gate-delay, or 3 units;
;;  Signal E: changes after 1 and-gate-delay + 1 inverter-delay, or 3+2 = 5 units;
;;  Signal S: the inputs to S are D and E, and both of these signals 
;;            change after 5 units, so the signal at S changes at 1 and-date-delay
;;            after this, or an additional 3 units after. The delay for the signal
;;            at S to change is therefore 5 + 3 = 8 units;
;; 
;; This matches the time delay we measure in the signal at S in the above.
;; Note that the signal at C did not change in this experiment.
;;

(get-signal a)
;; ==> 1
(get-signal b)
;; ==> 0
(get-signal s)
;; ==> 1
(get-signal c)
;; ==> 0

;;
;; This arrangement of signals indicates that 1 + 0 = 01. 
;;

(set-signal! b 1)
(propagate)
;; ==> c 11 New-value = 1
;; ==> s 16 New-value = 0

;;
;; The signal at S changes at time = 16, or 8 units after the signal 
;; at B changed to 1. This correlates with our analysis above where
;; it takes a time delay of 8 units for changes in the signal at either 
;; A or B to reach S. 
;; 
;; Likewise, according to the above analysis, the signal at C changes 1 
;; and-gate-delay after the signals at A or B change, or 3 time units 
;; later. In the above experiment, the signal at C changed at time = 11 
;; units, or 3 units after the signal at B changed to 1, which is again 
;; consistent with our analysis.
;;

(get-signal a)
;; ==> 1
(get-signal b)
;; ==> 1
(get-signal s)
;; ==> 0
(get-signal c)
;; ==> 1

;;
;; This arrangement of signals indicates that 1 + 1 = 10.
;;

(set-signal! a 0)
(propagate)
;; c 19 New-value = 0
;; s 24 New-value = 1

;; 
;; The time delays at C and S are consistent w/ our previous analysis.
;;

(get-signal a)
;; ==> 0
(get-signal b)
;; ==> 1
(get-signal s)
;; ==> 1
(get-signal c)
;; ==> 0

;;
;; This arrangement of signals indicates that 0 + 1 = 01.
;;

(set-signal! b 0)
(propagate)
;; ==> s 32 New-value 0

;;
;; The time delays at C and S are consistent w/ our previous analysis.
;;

(get-signal a)
;; ==> 0
(get-signal b)
;; ==> 0
(get-signal s)
;; ==> 0
(get-signal c)
;; ==> 0

;;
;; This arrangement of signals indicates that 0 + 0 = 00.
;;

;;
;; We next define the full-adder procedure:
;;
(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

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

;;
;; Run some unit tests for the full-adder:
;;
(define a (make-wire))
(define b (make-wire))
(define c-in (make-wire))
(define sum (make-wire))
(define c-out (make-wire))

(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(probe 'sum sum)
;; ==> 0 New-value = 0
(probe 'c-out c-out)
;; ==> 0 New-value = 0

(full-adder a b c-in sum c-out)
;; ==> ok

(get-signal a)
;; ==> 0
(get-signal b)
;; ==> 0
(get-signal c-in)
;; ==> 0
(get-signal sum)
;; ==> 0
(get-signal c-out)
;; ==> 0

(set-signal! a 1)
(propagate)
;; ==> sum 8 New-value = 1

;;
;; Setting the signal at A to 1 causes the signal at SUM to go to 1 as well,
;; after a delay of 8 time units. A time-analysis of how signals propagate
;; within the full-adder is as follows:
;;
;;  Signal A: Changing the signal at A causes a change in the signal at SUM
;;            one half-adder-sum-delay later; using the constants we have 
;;            defined here, this will occur 8 time-units later. Likewise, 
;;            changing the signal at A causes the signal at C-OUT to change
;;            one half-adder-carry-delay, plus one or-gate-delay later.
;;            Using the constants we have defined here, this will occur
;;            3 + 5 = 8 time units later. 
;;
;;  Signal B: Changing the signal at B causes a change in the signal at SUM
;;            two half-adder-sum-delays later; using the constants we have
;;            defined here, this will occur 16 time-units later. Likewise, 
;;            changing the signal at B causes the signal at C-OUT to change
;;            one half-adder-sum-delay, plus one-half-adder-carry-delay,
;;            plus one or-gate-delay later. Using the constants we have 
;;            defined here, this will occur 8 + 3 + 5 = 16 time units later.
;;
;;  Signal C-IN: Changing the signal at C-IN causes a change in the signal 
;;               at SUM to occur two half-adder-sum-delays later; using the 
;;               constants we have defined here, this will occur 16 time-units
;;               later. Likewise, changing the signal at C-IN will cause the 
;;               signal at C-OUT to change one half-adder-carry-delay, plus
;;               one or-gate delay later. Using the constants we have defined
;;               here, this will occur 3 + 5 = 8 time units later.
;;

(get-signal a)
;; ==> 1
(get-signal b)
;; ==> 0
(get-signal c-in)
;; ==> 0
(get-signal sum)
;; ==> 1
(get-signal c-out)
;; ==> 0

;;
;; This arrangement of signals indicates that 1 + 0 + 0 = 01.
;;

(set-signal! b 1)
(propagate)
;; ==> c-out 24 New-value = 1
;; ==> sum 24 New-value = 0

;;
;; The time delays at SUM and C-OUT are consistent w/ our previous analysis.
;;

(get-signal a)
;; ==> 1
(get-signal b)
;; ==> 1
(get-signal c-in)
;; ==> 0
(get-signal sum)
;; ==> 0
(get-signal c-out)
;; ==> 1

;;
;; This arrangement of signals indicates that 1 + 1 + 0 = 10.
;;

(set-signal! a 0)
(propagate)
;; ==> c-out 32 New-value = 0
;; ==> sum 32 New-value = 1

;; 
;; The time delays at SUM and C-OUT are consistent w/ our previous analysis.
;;

(get-signal a)
;; ==> 0
(get-signal b)
;; ==> 1
(get-signal c-in)
;; ==> 0
(get-signal sum)
;; ==> 1
(get-signal c-out)
;; ==> 0

;;
;; This arrangement of signals indicates that 0 + 1 + 0 = 01.
;;

(set-signal! c-in 1)
(propagate)
;; ==> c-out 40 New-value = 1
;; ==> sum 48 New-value = 0

;;
;; The time delays at SUM and C-OUT are consistent w/ our previous analysis.
;;

(get-signal a)
;; ==> 0
(get-signal b)
;; ==> 1
(get-signal c-in)
;; ==> 1
(get-signal sum)
;; ==> 0
(get-signal c-out)
;; ==> 1

;;
;; This arrangement of signals indicates that 0 + 1 + 1 = 10.
;;

(set-signal! a 1)
(propagate)
;; sum 56 New-value = 1

;; 
;; The time delays at SUM is consistent w/ our previous analysis.
;;

(get-signal a)
;; ==> 1
(get-signal b)
;; ==> 1
(get-signal c-in)
;; ==> 1
(get-signal sum)
;; ==> 1
(get-signal c-out)
;; ==> 1

;;
;; This arrangement of signals indicates that 1 + 1 + 1 = 11.
;;

(set-signal! b 0)
(propagate)
;; c-out 64 New-value = 0
;; c-out 72 New-value = 1
;; sum 72 New-value = 0

;;
;; Looking at the definition for the full-adder, when the signals at 
;; A, B and C-IN are all 1, the signals on the internal components 
;; have the following values:
;;
;;  S:  0
;;  C1: 1
;;  C2: 0
;;  
;; This in turn drives the signals at SUM and C-OUT to both be 1.
;;
;; When the signal at B is changed from 1 to 0, this drives the signal
;; at C1 to 0 one half-adder-carry-delay, or 3 time units later. This 
;; change in turn drives the signal at C-OUT to 0 one or-gate-delay, 
;; or 5 time units later. Hence, changing the signal at B from 1 to 0 
;; drives the signal at C-OUT to 0 after 8 time units, or time = 64.
;;
;; However, changing the signal at B to 0 also drives the signal at S, 
;; which is the input to the second half-adder, to 1. This in turn drives 
;; the signal at SUM to 0 and the signal at C-OUT to 1. The change in signal 
;; at S occurs 8 time units after the signal at B has changed. This in 
;; turn drives a change in the signal at SUM another 8 time units after 
;; that, or in other words, the signal at SUM changes to 0 a total of 
;; 16 time-units after the signal at B was changed to 0.
;;
;; Likewise, changing the signal at B to 0 drives the signal at S to 1
;; after 8 time units. The change at S in turn drives the signal at SUM
;; to 0 after an additional 8 time units. So changing the signal at B from
;; 1 to 0 changes the signal to SUM to 0 after 16 time units, and changes
;; the signal at C-OUT to 1 after 16 time units.
;;
;; The behavior we observe in this experiment is consistent with this analysis.
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
