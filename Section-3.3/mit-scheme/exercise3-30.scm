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
;; | A | B | D | E | C | S |
;; +---+---+---+---+---+---+
;; | 1 | 1 | 1 | 0 | 1 | 0 |
;; | 1 | 0 | 1 | 1 | 0 | 1 |
;; | 0 | 1 | 1 | 1 | 0 | 1 |
;; | 0 | 0 | 0 | 1 | 0 | 0 |
;; +---+---+---+---+---+---+
;;
;; C is 1 whenever both A and B are 1 (logical-and), and S is 1 whenever 
;; precisely one of A and B is 1 (exclusive-or).
;;
;; The half-adder defines a mechanism for adding two bits, A and B, 
;; to obtain a 2-bit number as specified by the sum and carry bits:
;;
;; +--------------+
;; | A | B | | CS |
;; +--------------+
;;   1 + 1 ==> 10
;;   1 + 0 ==> 01
;;   0 + 1 ==> 01
;;   0 + 0 ==> 00 
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
;;  Signal E: changes after 1 and-gate-delay + 1 inverter-delay, or 3 + 2 = 5 units;
;;  Signal S: the inputs to S are D and E, and both of these signals 
;;            change after 5 units, so the signal at S changes at 1 and-gate-delay
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
;; Setting the signal on B to 1 causes the signal at C to go to 1, 
;; and the signal at S to go from 1 to 0. A time-analysis of how 
;; signals propagate within the half-adder is as follows:
;;
;;  Signal D: remains at 1, since D is the logical-or of inputs 
;;            A and B, which are both 1;
;;  Signal C: changes to 1, since C is the logical-and of inputs
;;            A and B, which are both 1; the change occurs after 
;;            1 and-gate-delay, or 3 units, or at t = 11;
;;  Signal E: changes to 0, since E is the logical-not of C; the 
;;            change occurs after 1 and-gate-delay + 1 inverter-
;;            delay, or 3+2 = 5 units, or t = 13;
;;  Signal S: changes to 0, since S is the logical-and of inputs 
;;            D and E; the signal at D did not change, and the 
;;            signal at E changes to 0 at t(E) = 5 units, hence 
;;            the signal at S changes at t(E)+1 and-gate-delay, 
;;            or after 5+3 = 8 units, or t = 16;
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

;; Clear out agenda
(define the-agenda (make-agenda))
(current-time the-agenda)
;; ==> 0

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
;; since SUM is the sum of the input signals S and A. A time-analysis of how
;; signals propagate within the full-adder is as follows:
;;
;;  Signal S:     does not change;
;;  Signal C1:    does not change;
;;  Signal C2:    does not change;
;;  Signal SUM:   changes to 1 after 1 half-adder-sum-delay, which using the 
;;                gate delays we have defined here, is 8 units;
;;  Signal C-OUT: does not change;
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
;; A time-analysis of how these signals propagate within the full
;; adder is as follows:
;;
;;  Signal S:     changes to 1 after 1 half-adder-sum-delay, which is 8 units;
;;  Signal C1:    does not change;
;;  Signal C2:    changes to 1 at 1 half-adder-carry-delay after the signal
;;                at input S has changed to 1, which is 3 units, so the signal
;;                at C2 changes 8+3 = 11 units after the signal at B changes;
;;  Signal SUM:   changes to 0 at 1 half-adder-sum-delay after the signal 
;;                at input S has changed to 1, which is 8 units; so the signal 
;;                at SUM changes 8+8 = 16 units after the siganl at B changes, 
;;                or at t = 24;
;;  Signal C-OUT: changes to 1 since C-OUT is the logical-or of C1 and C2, and
;;                and the change occurs 1 or-gate-delay after the signal at C2
;;                has chaged, which is 5 units; hence, the signal at C-OUT
;;                changes (8+3)+5 = 16 units after the signal at B has changed,
;;                or at t = 24.
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
;; We showed above that changing the signal at A will propagate to SUM
;; after 8 units. 
;;
;; To calculate the time it takes a signal change at A to propagate to 
;; C-OUT, note that the signal change at A will drive a signal change at
;; C2 after 1 half-adder-carry-delay, or 3 units. Likewise, the signal
;; change at C2 will drive a signal change at C-OUT after 1 or-gate-delay, 
;; or 5 units. 
;;
;; Hence, the total time for the signal change at A to propagate to C-OUT 
;; is 5+3 = 8 units, consistent with the experiment above.
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
;; A time-analysis of how these signals propagate within the full
;; adder is as follows:
;;
;;  Signal S:     changes to 0 after 1 half-adder-sum-delay, which is
;;                8 units;
;;  Signal C1:    changes to 1 after 1 half-adder-carry-delay, which is 
;;                3 units; 
;;  Signal C2:    does not change;
;;  Signal SUM:   changes to 0 at 1 half-adder-sum-delay after the signal
;;                at S has changed, which is 8 units; hence, the signal 
;;                at SUM changes 8+8 = 16 units after the signal at C has
;;                changed, or at time t = 48;
;;  Signal C-OUT: changes to 1 at 1 or-gate-delay after the input signals
;;                C1 and C2 have obtained their final value; the signal 
;;                C1 changes value 3 units after the signal at C changed, 
;;                and the signal C2 does not change; hence, the signal at 
;;                C-OUT changes at 3+5 = 8 units after the signal at C 
;;                has changed, or at t = 40;
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
;; A, B and C-IN are all 1, the signals on the internal wires have 
;; the following values:
;;
;;  S:  0
;;  C1: 1
;;  C2: 0
;; 
;; These internal values, in turn, drive the signals at SUM and C-OUT
;; to both be 1. 
;;
;; When the signal at B is changes from 1 to 0, this drives the signal
;; at C1 to 0 one half-adder-carry-delay; this change in turn drives the 
;; signal at C-OUT to 0 after one or-gate-delay. Using the constants we 
;; have defined here, this will occur 3 + 5 = 8 time units later, or at 
;; time = 64.
;;
;; However, changing the signal at B to 0 also drives the signal at S, 
;; which is the input to the second half-adder, to 1; this in turn drives 
;; the signal at SUM to 0. The change at S occurs one half-adder-sum-delay,
;; or 8 time units, after the signal change at B occurred; the change at SUM
;; occurs one half-adder-sum-delay, or 8 time units, after the signal change
;; at S occurred. Hence, a total of 8 + 8 = 16 time units elapse between 
;; when the signal at B changes and the change propagates to SUM. We observe
;; the signal change at SUM at time = 72. 
;;
;; Likewise, driving the signal at S to 1 will in turn drive the signal at 
;; C2 from 0 to 1; this, in turn, drives the signal at C-OUT to 1. The change
;; at C2 will occur one half-adder-carry-delay, or 3 time units, after the 
;; signal at S changes; the change at C-OUT will occur one or-gate-delay, or
;; 5 time units, after the signal at C2 changes. Hence, the change a C-OUT 
;; will occur 3 + 5 = 8 time units after the signal at S changes, and the 
;; signal at S changed 8 time units after the signal at B changed. Hence, 
;; changing the signal at B from 1 to 0 drives the signal at C-OUT to 1 a 
;; total of 16 time units later, or at time = 72.
;;
;; The behavior we observe in this experiment is consistent with this analysis.
;;

(get-signal a)
;; ==> 1
(get-signal b)
;; ==> 0
(get-signal c-in)
;; ==> 1
(get-signal sum)
;; ==> 0
(get-signal c-out)
;; ==> 1

;;
;; This arrangement of signals indicates that 1 + 0 + 1 = 10.
;;

(set-signal! a 0)
(propagate)
;; c-out 80 New-value = 0
;; sum 80 New-value 1

;;
;; The time delays at SUM and C-OUT are consistent w/ our analysis above.
;;

(get-signal a)
;; ==> 0
(get-signal b)
;; ==> 0
(get-signal c-in)
;; ==> 1
(get-signal sum)
;; ==> 1
(get-signal c-out)
;; ==> 0

;;
;; This arrangement of signals indicates that 0 + 0 + 1 = 01.
;;

(set-signal! c-in 0)
(propagate)
;; sum 96 New-value: 0

;; 
;; The change at C-IN drives a change at C1 after one half-adder-carry-delay, 
;; or 3 time units; the change at C1 in turn drives a change at C-OUT after 
;; one or-gate-delay, or 3 time units. A total of 6 time units elapse between 
;; the change in signal at C-IN and when the change propagates to C-OUT.
;;

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

;; 
;; This arrangement of signals indicates that 0 + 0 + 0 = 00.
;;

;;
;; We can define the ripple carry adder as followS:
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
  
;;
;; The ripple carry adder allows us to add two n-bit integers, together with
;; a carry bit. Owing to the way the procedure is designed, we cannot probe 
;; the final output carry signal, although we can probe all n sum signals. 
;;


;; [WORKING]

;;
;; We can calculate the time delay that it takes for a signal to propagate 
;; all the way down an n-bit ripple carry adder as follows:
;;
;; The half-adder circuit is designed to drive two output signals, S and C, 
;; from two input signals, A and B. The half-adder also contains two internal
;; wires, D and E; D is the logical-and of A and B, while E is the logical-not
;; of C. The output signal S, in turn, is driven from the logical-and of D
;; and E. 
;;
;; The signal change propagates to C one and-gate-delay after the signals at
;; A and/or B have changed. 
;;
;; The signal change propagates to D one or-gate-delay after the signals at 
;; A and/or B have changed. Likewise, the signal change propagates to E one
;; and-date-delay plus one inverter-delay after the signals at A and/or B have
;; changed. The signal change propagates to S one and-gate-delay after the 
;; signals at D and/or E have changed. 
;;
;; Hence, for a half-adder, the propagation delays look like:
;;
;;  C(1/2)_time = or-gate-delay
;;  S(1/2)_time = MAX(or-gate-delay, and-gate-delay + inverter-delay) + and-gate-delay
;;
;; The full-adder circuit is designed to drive two output signals, SUM and C-OUT,
;; from three input signals, A, B and C-IN. The full-adder also contains three 
;; internal wires, S, C1 and C2; C1 is the half-adder carry signal from B and C-IN;
;; S is the half-adder sum signal from B and C-IN; and C2 is the half-adder carry 
;; signal from A and S. C-OUT, in turn, is driven from the logical-or of C2 and 
;; C1; and SUM is the half-adder sum driven from A and S.
;;
;; We have:
;;
;;  C1_time = C(1/2)_time 
;;  S_time = S(1/2)_time 
;;  C2_time = S(1/2)_time + C(1/2)_time
;;
;; so that:
;;
;;  C-OUT(1)_time = MAX(C1_time, C2_time) + or-gate-delay
;;  SUM(1)_time = S_time + S(1/2)_time
;;
;; Reducing these expressions, we have:
;;
;;  C-OUT(1)_time = MAX(C(1/2)_time, S(1/2)_time + C(1/2)_time) + or-gate-delay
;;  C-OUT(1)_time = MAX(or-gate-delay, MAX(or-gate-delay, and-gate-delay + inverter-delay) + and-gate-delay) + or-gate-delay
;;
;;  SUM(1)_time = S(1/2)_time + S(1/2)_time 
;;  SUM(1)_time = 2 * MAX(or-gate-delay, and-gate-delay + inverter-delay) + 2 * and-gate-delay
;;
;; Forming these expressions in Lisp:
;;
(define c-out-delay (+ (max 
			or-gate-delay
			(max or-gate-delay (+ and-gate-delay inverter-delay)))
		       or-gate-delay))

(define sum-1-delay (* 2 (+ (max or-gate-delay (+ and-gate-delay inverter-delay))
			  and-gate-delay)))

;;
;; For a n-bit ripple carry adder, the signal at S(n) cannot be calculated until the 
;; carry signal from C(n-1) has been propagated. Hence, the total time will be:
;; (total time is the delay for the carry signal to arrive, plus the full-adder delay 
;; of getting the sum time).
;;
;;  SUM(N)_time = N * C-OUT(1)_time + SUM(1)_time 
;; 
(define sum-delay (n)
  (+ (* n c-out-delay)
     sum-1-delay))
