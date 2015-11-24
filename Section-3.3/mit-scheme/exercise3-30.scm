;;
;; [Exercise 3.30]
;;
;; Figure 3.27 shows a ripple-carry adder formed by stringing together n 
;; full-adders. This is the simplest form of parallel adder for adding 
;; two n-bit binary numbers. The inputs A1, A2, A3, ..., A(N) and B1, B2, 
;; B3, ..., B(N) are the two binary numbers to be added (each A(k) and B(k) 
;; is a 0 or a 1). The circuit generates S1, S2, S3, ..., SN, the n bits of 
;; the sum, and C, the carry from the addition. Write a procedure 
;; ripple-carry-adder that genreates this circuit. The procedure should
;; take as arguments three lists of n wires each -- and A(k), the B(k) and
;; the S(k) -- and also another wire C. The major drawback of the ripple-carry
;; adder is the need to wait for the carry signals to propagate. What is
;; the delay needed to obtain the complete output from an n-bit ripple-carry
;; adder, expressed in terms of the delays for and-gates, or-gates and inverters?
;;
;;        A1 B1           A2 B2           A3 B3
;;        |  |    C1      |  |    C2      |  |    C3
;;        |  |  +----+    |  |  +----+    |  |  +----
;;        |  |  |    |    |  |  |    |    |  |  |
;;      +-+--+--+-+  |  +-+--+--+-+  |  +-+--+--+-+
;;      |         |  |  |         |  |  |         |
;;      |         |  |  |         |  |  |         |
;;      |         |  |  |         |  |  |         |
;;      |         |  |  |         |  |  |         |
;;      +-+-----+-+  |  +-+-----+-+  |  +-+-----+-+
;;        |     |    |    |     |    |    |     |
;;        |     |    |    |     |    |    |     |
;; C -----+     |    +----+     |    +----+     |
;;              S1              S2              S3
;;

;; [WORKING]

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
;; Setting the signal on A to 1 drives the signal at S to go to 1 as well,
;; after a delay of 8 time units. A time-analysis of how signals propagate 
;; within the half-adder is as follows:
;;
;;  Signal D: Changes after 1 or-gate-delay, or 5 units.
;;  Signal C: Changes after 1 and-gate-delay, or 3 units.
;;  Signal E: Changes after 1 and-gate-delay + 1 inverter-delay, or 3 + 2 = 5 units.
;;  Signal S: The inputs to S are D and E, and both of these signals 
;;            change after 5 units, so the signal at S changes at 1 and-gate-delay
;;            after this, or an additional 3 units after. The delay for the signal
;;            at S to change is therefore 5 + 3 = 8 units.
;; 
;; This matches the time delay we measure in the signal at S in the above.
;; Note that the signal at C did not change in this simulation.
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
;;  Signal D: Remains at 1, since D is the logical-or of inputs 
;;            A and B, which are both 1.
;;  Signal C: Changes to 1, since C is the logical-and of inputs
;;            A and B, which are both 1; the change occurs after 
;;            1 and-gate-delay, or 3 units, or at t = 11.
;;  Signal E: Changes to 0, since E is the logical-not of C; the 
;;            change occurs after 1 and-gate-delay + 1 inverter-
;;            delay, or 3+2 = 5 units, or t = 13.
;;  Signal S: Changes to 0, since S is the logical-and of inputs 
;;            D and E; the signal at D did not change, and the 
;;            signal at E changes to 0 at t(E) = 5 units, hence 
;;            the signal at S changes at t(E)+1 and-gate-delay, 
;;            or after 5+3 = 8 units, or t = 16.
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
;;  Signal S:     Does not change.
;;  Signal C1:    Does not change.
;;  Signal C2:    Does not change.
;;  Signal SUM:   Changes to 1 after 1 half-adder-sum-delay, which using the 
;;                gate delays we have defined here, is 8 units.
;;  Signal C-OUT: Does not change.
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
;;  Signal S:     Changes to 1 after 1 half-adder-sum-delay, which is 8 units.
;;  Signal C1:    Does not change.
;;  Signal C2:    Changes to 1 at 1 half-adder-carry-delay after the signal
;;                at input S has changed to 1, which is 3 units, so the signal
;;                at C2 changes 8+3 = 11 units after the signal at B changes.
;;  Signal SUM:   Changes to 0 at 1 half-adder-sum-delay after the signal
;;                at input S has changed to 1, which is 8 units; so the signal 
;;                at SUM changes 8+8 = 16 units after the siganl at B changes, 
;;                or at t = 24.
;;  Signal C-OUT: Changes to 1 since C-OUT is the logical-or of C1 and C2, and
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
;; is 5+3 = 8 units, consistent with the simulation above.
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
;;  Signal S:     Changes to 0 after 1 half-adder-sum-delay, which is
;;                8 units.
;;  Signal C1:    Changes to 1 after 1 half-adder-carry-delay, which is 
;;                3 units. 
;;  Signal C2:    Does not change.
;;  Signal SUM:   Changes to 0 at 1 half-adder-sum-delay after the signal
;;                at S has changed, which is 8 units; hence, the signal 
;;                at SUM changes 8+8 = 16 units after the signal at C has
;;                changed, or at time t = 48.
;;  Signal C-OUT: Changes to 1 at 1 or-gate-delay after the input signals
;;                C1 and C2 have obtained their final value; the signal 
;;                C1 changes value 3 units after the signal at C changed, 
;;                and the signal C2 does not change; hence, the signal at 
;;                C-OUT changes at 3+5 = 8 units after the signal at C 
;;                has changed, or at t = 40.
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
;; The time delays at SUM are consistent w/ our previous analysis.
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
;; A time analysis of how these signals propagate within the 
;; full adder is shown below:
;;
;;  Signal S:     Changes to 1 at 1 half-adder-sum-delay after 
;;                the signal at B changes, which is 8 units.
;;  Signal C1:    Changes to 0 at 1 half-adder-carry-delay after
;;                the signal at B changes, which is 3 units.
;;  Signal C2:    Changes to 1 at 1 half-adder-carry-delay after
;;                the signal at S changes, which is 3 units; hence, 
;;                the signal at C2 changes 8+3 = 11 units after the 
;;                the signal at B changes.
;;  Signal SUM:   Changes to 0 at 1 half-adder-sum-delay after 
;;                the signal at S changes, which is 8 units; hence
;;                the signal at SUM changes 8+8 = 16 units after 
;;                the signal at B changes, or at t = 72.
;;  Signal C-OUT: Changes value at 1 or-gate-delay after the signals 
;;                at inputs C1 or C2 change, which is 5 units. When 
;;                the inputs A, B and C-IN are all 1, the values of 
;;                C1 and C2 are 1 and 0, respectively, and hence the 
;;                value of C-OUT, which is the logical-or of C1 and
;;                C2, is 1. When the signal at B changes to 0, it 
;;                drives the signal at C1 to change to 0 after 3 units, 
;;                which turn drives the signal at C-OUT to change to 0 
;;                after an additional or-gate-delay, or 5 units; hence, 
;;                after 3+5 = 8 units, or at t = 64, the signal at C-OUT 
;;                changes to 0.
;;                Likewise, setting B to 0 drives the signal at C2 to 
;;                change to 1 after 11 units, so that an additional 5 
;;                units later, or after 11+5 = 16 units, or at t = 72, 
;;                the signal at C-OUT goes back to 1.
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
;; We can define the ripple carry adder as follows:
;;
(define (ripple-carry-adder a-list b-list c s-list)
  (define (ripple-carry-adder-iter a b c-in s)
    (let ((a1 (car a))
	  (b1 (car b))
	  (s1 (car s)))
      (if (and (not (null? a1))
	       (not (null? b1))
	       (not (null? s1)))
	  (let ((c-out (make-wire)))
	    (full-adder a1 b1 c-in s1 c-out)
	    (let ((an (cdr a))
		  (bn (cdr b))
		  (sn (cdr s)))
	      (if (and (not (null? an))
		       (not (null? bn))
		       (not (null? sn)))
		  (ripple-carry-adder-iter an bn c-out sn)
		  'ok)))
	  (error "Bad inputs: " a1 b1 s1))))
  (ripple-carry-adder-iter a-list b-list c s-list))
  
;;
;; The ripple carry adder allows us to add two n-bit integers, together with
;; a carry bit. Owing to the way the procedure is designed, we cannot probe 
;; the final output carry signal, although we can probe all n sum signals. 
;;

;;
;; The half-adder circuit is desigend to drive two output signs, S and C,
;; from two input signals, A and B. The half-adder also contains two internal
;; wires, D and E:
;;
;;  Signal D: Logical-or of A and B.
;;  Signal E: Logical-not of C.
;;
;; The output signals S and C are defined as:
;;
;;  Signal S: Logical-and of D and E.
;;  Signal C: Logical-and of A and B.
;;
;; An analysis of the time it takes for a singal to propagate all the way 
;; down an n-bit ripple carry adder is as follows:
;;
;;  Signal C: The signal change propagates to C at 1 and-gate-delay after the 
;;            input signals at A and/or B have changed.
;;  Signal D: The signal change propagates to D at 1 or-gate-delay after the 
;;            input signals at A and/or B have changed.
;;  Signal E: The signal change propagates to E at 1 and-gate-delay (signal C) 
;;            plus 1 inverter-delay after the input signals at A and/or B have 
;;            changed.
;;  Signal S: The signal change propagates to S at 1 and-gate-delay after the 
;;            input signals at D and/or E have changed.
;;
;; The output signal propagation delays for the half-adder are given below. 
;; Note that the maximum propagation delay at S is gated by which signal arrives
;; last - the logical-or of A and B (i.e., input D) or the logical-not of the 
;; logical-and of A and B (i.e., input E):
;;
(define c-half-adder-delay and-gate-delay)
(define s-half-adder-delay (+ and-gate-delay
			      (max or-gate-delay
				   (+ and-gate-delay inverter-delay))))

;;
;; Using the values we have defined, these expressions evaluate to the following:
;;
c-half-adder-delay
;; ==> 3
s-half-adder-delay 
;; ==> 8

;;
;; This is consistent with the results of the simulations we run above.
;;

;;
;; The full-adder circuit is designed to drive two output signals, SUM and C-OUT,
;; from three input signals, A, B and C-IN. The full-adder also contains three 
;; internal wires, S, C1 and C2:
;; 
;;  Signal S:  Half-adder sum signal of B and C-IN.
;;  Signal C1: Half-adder carry signal of B and C-IN.
;;  Signal C2: Half-adder carry signal of A and S.
;;
;; The output signals SUM and C-OUT are defined as:
;;
;;  Signal SUM:   Half-adder sum signal of A and S.
;;  Signal C-OUT: Logical-or of C1 and C2.
;;
;; An analysis of the time it takes for a signal to propagate all the way down 
;; an n-bit ripple carry adder is as follows:
;;
;;  Signal S:     The signal change propagates to S at 1 half-adder-sum-delay 
;;                after the input signals at A and/or B have changed.
;;  Signal C1:    The signal change propagates to C1 at 1 half-adder-carry-
;;                delay after the input signals at A and/or B have changed.
;;  Signal C2:    The signal change propagates to C2 at 1 half-adder-carry-
;;                delay after the input signals at A and/or S have changed.
;;  Signal SUM:   The signal change propagates to SUM at 1 half-adder-sum-
;;                delay after the input signals at A and/or S have changed.
;;  Signal C-OUT: The signal change propagates to C-OUT at 1 or-gate-delay
;;                after the input signals at C1 and/or C2 have changed.
;;
;; The output signal propagation delays for the full-adder are given below:
;;
(define s-full-adder-delay     s-half-adder-delay)
(define c1-full-adder-delay    c-half-adder-delay)
(define c2-full-adder-delay    (+ s-full-adder-delay c-half-adder-delay))
(define sum-full-adder-delay   (+ s-full-adder-delay s-half-adder-delay))
(define c-out-full-adder-delay (+ or-gate-delay
				  (max c1-full-adder-delay
				       c2-full-adder-delay)))

;;
;; In the case of the propagation delay at C-OUT, we will always have 
;; c2-full-adder-delay >= c1-full-adder-delay, so we can just write:
;;
(define c-out-full-adder-delay (+ or-gate-delay c2-full-adder-delay))

;;
;; Using the values we have defined, these expressions evaluate to the following:
;;
sum-full-adder-delay
;; ==> 16 
c-out-full-adder-delay
;; ==> 16

;; 
;; These values are consistent with the maximum propagation delays that we observed
;; in the simulations above.
;;

;;
;; Consider next the n-th full adder in an n-bit ripple carry adder. The values
;; taken on by the signals at C(n) and S(n) are gated by the time it takes the 
;; late-arriving signal from C(n-1) to arrive. 
;;
;; We can define the propagation delays at S(n) and C(n) in terms of a single-
;; valued function that takes one argument, n:
;;
(define (s-ripple-delay n)
  (if (= n 1)
      sum-full-adder-delay
      (+ sum-full-adder-delay
	 (c-ripple-delay (- n 1)))))

(define (c-ripple-delay n)
  (if (= n 1)
      c-out-full-adder-delay
      (+ c-out-full-adder-delay
	 (c-ripple-delay (- n 1)))))

;;
;; These expressions can be rewritten more succinctly as:
;;
(define (s-ripple-delay n)
  (+ sum-full-adder-delay (* (- n 1) c-out-full-adder-delay)))

(define (c-ripple-delay n)
  (* n c-out-full-adder-delay))

;; 
;; We can express sum-full-adder-delay in terms of more basic values, like the 
;; propagation delays for and-gates, or-gates and inverters. For now, we will 
;; not reduce the expression for s-half-adder-delay since it contains a "max" conditional:
;;
(define sum-full-adder-delay (+ s-full-adder-delay 
				s-half-adder-delay))
(define sum-full-adder-delay (+ s-half-adder-delay
				s-half-adder-delay))
(define sum-full-adder-delay (* 2 s-half-adder-delay))

;;
;; We can perform the same reduction for c-out-full-adder-delay:
;;
(define c-out-full-adder-delay (+ or-gate-delay
				  c2-full-adder-delay))
(define c-out-full-adder-delay (+ or-gate-delay
				  (+ s-full-adder-delay 
				     c-half-adder-delay)))
(define c-out-full-adder-delay (+ or-gate-delay
				  (+ s-half-adder-delay
				     c-half-adder-delay)))
(define c-out-full-adder-delay (+ or-gate-delay
				  c-half-adder-delay
				  s-half-adder-delay))
(define c-out-full-adder-delay (+ or-gate-delay
				  and-gate-delay
				  s-half-adder-delay))

;;
;; Using the values we have defined, these expressions evaluate to the following:
;;
sum-full-adder-delay
;; ==> 16
c-out-full-adder-delay
;; ==> 16

;;
;; This is consistent with the answers previously obtained.
;;

;;
;; With these expressions, we can write simplified expressions for s-ripple-delay:
;;
(define (s-ripple-delay n)
  (+ (* 2 s-half-adder-delay)
     (* (- n 1) (+ or-gate-delay
		   and-gate-delay
		   s-half-adder-delay))))

(define (s-ripple-delay n)
  (+ (* (+ n 1) s-half-adder-delay)
     (* (- n 1) (+ or-gate-delay and-gate-delay))))

;;
;; and for c-ripple-delay:
;;
(define (c-ripple-delay n)
  (* n (+ or-gate-delay
	  and-gate-delay
	  s-half-adder-delay)))

(define (c-ripple-delay n)
  (+ (* n s-half-adder-delay)
     (* n (+ or-gate-delay and-gate-delay))))

;;
;; Using the values we have defined, these expressions evaluate to the following:
;;
(s-ripple-delay 1)
;; ==> 16
(s-ripple-delay 2)
;; ==> 32
(s-ripple-delay 3)
;; ==> 48

(c-ripple-delay 1)
;; ==> 16
(c-ripple-delay 2)
;; ==> 32
(c-ripple-delay 3)
;; ==> 48

;;
;; Using the definition for ripple-carry-adder above, we can run a set of 
;; simulations to test the expressions we have derived above. We will model the 
;; simulation using a 3-bit ripple carry adder, and run the simulation by starting
;; with all the inputs set to 0, and then forcing all the inputs to 1 simultaneously, 
;; thus triggering the "worst case" scenario of maximum propagation delay.
;;

;; Clear out agenda:
(define the-agenda (make-agenda))
(current-time the-agenda)
;; ==> 0

;; Define wires sets:
(define a1 (make-wire))
(define a2 (make-wire))
(define a3 (make-wire))
(define a-inputs (list a1 a2 a3))

(define b1 (make-wire))
(define b2 (make-wire))
(define b3 (make-wire))
(define b-inputs (list b1 b2 b3))

(define s1 (make-wire))
(define s2 (make-wire))
(define s3 (make-wire))
(define s-inputs (list s1 s2 s3))

(define c-in (make-wire))

;; Configure the probes:
(probe 's1 s1)
;; ==> s1 0 New-value = 0
(probe 's2 s2)
;; ==> s2 0 New-value = 0
(probe 's3 s3)
;; ==> s3 0 New-value = 0

;; Configure ripple carry adder:
(ripple-carry-adder a-inputs b-inputs c-in s-inputs)

;;
;; Not all signal change propagations will take the maximum amount
;; of time to propagate down to the n-th full adder. The following
;; combination of signals should cause the signal at S3 to change 
;; at the maximum propagation time:
;;
(set-signal! a1 1)
(set-signal! a2 1)
(set-signal! c-in 1)

(propagate)
;; s1 8 New-value = 1
;; s2 8 New-value = 1
;; s1 16 New-value = 0
;; s2 32 New-value = 0
;; s3 48 New-value = 1

;;
;; A time analysis of the propagation delays in the first full adder is as follows.
;; We indicate an entry with (**) if it was reported in the probe trace above. We 
;; indicate with (') an entry if it is an internal connection in the full adder:
;;
;;  S1:       Changes to 1 at 8 units after, or t = 8, the signal at A1 
;;            changed to 1 (**).
;;  SUM-1':   Changes to 1 at 8 units after, or t = 8, the signal at C-IN 
;;            changed to 1. 
;;  C2-1':    Changes to 1 at 3 units after, or t = 11, the signal at SUM-1'
;;            changed to 1. 
;;  S1:       Changes to 0 at 8 units after, or t = 16, the signal at SUM-1'
;;            changed to 1 (**).
;;  C-OUT-1': Changes to 1 at 5 units after, or t = 16, the signal at C2-1' 
;;            changed to 1. 
;;
;; A time analysis of the propagation delays in the second full adder is as follows:
;;
;;  S2:       Changes to 1 at 8 units after, or t = 8, the signal at A2 
;;            changed to 1 (**).
;;  SUM-2':   Changes to 1 at 8 units after, or t = 24, the signal at C-OUT-1' 
;;            changed to 1.
;;  C2-2':    Changes to 1 at 3 units after, or t = 27, the signal at SUM-2' 
;;            changed to 1.
;;  S2:       Changes to 0 at 8 units after, or t = 32, the signal at SUM-2' 
;;            changed to 1 (**).
;;  C-OUT-2': Changes to 1 at 5 units after, or t = 32, the signal at C2-2'
;;            changed to 1.
;;
;; A time analysis of the propagation delays in the third full adder is as follows:
;;
;;  SUM-3': Changes to 1 at 8 units after, or t = 40, the signal at C-OUT-2'
;;          changed to 1.
;;  S3:     Changes to 1 at 8 units after, or t = 48, the signal at SUM-3'
;;          changed to 1.
;;
;; This is consistent with the simulation we run above.
;;

;;
;; As a final step, we reduce the expression for s-half-adder-delay, so that we can 
;; obtain an expression for (s-ripple-delay n) (we will ignore (c-ripple-delay n) 
;; since we cannot probe the last carry-out signal):
;;
(define s-half-adder-delay
  (if (> or-gate-delay (+ and-gate-delay inverter-delay))
      (+ and-gate-delay or-gate-delay)
      (+ (* and-gate-delay 2) inverter-delay)))

(define (s-ripple-delay n)
  (+ (* (+ n 1) s-half-adder-delay)
     (* (- n 1) (+ or-gate-delay and-gate-delay))))

(define (s-ripple-delay n)
  (+ (* (+ n 1) (if (> or-gate-delay (+ and-gate-delay inverter-delay))
		    (+ and-gate-delay or-gate-delay)
		    (+ (* and-gate-delay 2) inverter-delay)))
     (* (- n 1) (+ or-gate-delay and-gate-delay))))

;;
;; Let s-ripple-delay-1 be the delay in the case where or-gate-delay is greater
;; than (+ and-gate-delay inverter-delay):
;;
(define (s-ripple-delay-1 n)
  (+ (* (+ n 1) (+ and-gate-delay or-gate-delay))
     (* (- n 1) (+ and-gate-delay or-gate-delay))))

(define (s-ripple-delay-1 n)
  (* 2 n (+ and-gate-delay or-gate-delay)))

;;
;; Algebraically, this can be expressed as D(n) = 2 * n * (and-gate-delay + or-gate-delay).
;;

;;
;; Let s-ripple-delay-2 be the delay in the case where or-gate-delay is less than
;; or equal to (+ and-gate-delay or-gate-delay):
;; 
(define (s-ripple-delay-2 n)
  (+ (* (+ n 1) (+ (* and-gate-delay 2) inverter-delay))
     (* (- n 1) (+ and-gate-delay or-gate-delay))))

;;
;; Algebraically, this can be expressed as:
;;
;;   D(n) = (n+1) * (2 * and-gate-delay + inverter-delay) + (n-1) * (and-gate-delay + or-gate-delay).
;;