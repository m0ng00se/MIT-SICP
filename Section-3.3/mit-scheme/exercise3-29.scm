;;
;; Exercise 3.29
;;
;; Another way to construct an or-gate is as a compound digital logic device, 
;; built from and-gates and inverters. Define a procedure "or-gate" that 
;; accomplishes this. What is the delay time of the or-gate in terms of 
;; and-gate-delay and inverter-delay?
;;

;;
;; We can invert both inputs, attach them to an and-gate, and then invert
;; the output of the and-gate:
;;
;;              _
;;             | \ 
;;  INPUT 1 ---|  o---+
;;             |_/    |   +------)      _
;;                    +---|       )    | \
;;                        |        )---|  o--- OUTPUT 
;;              _     +---|       )    |_/
;;             | \    |   +------)
;;  INPUT 2 ---|  o---+
;;             |_/ 
;; 

;;
;; ------------------------------------------------------ 
;; | I1 | ~I1 | I2 | ~I2 | (~I1 && ~I2) | ~(~I1 && ~I2) |
;; ------------------------------------------------------ 
;; |  1 |  0  |  1 |  0  |       0      |        1      |
;; |  1 |  0  |  0 |  1  |       0      |        1      |
;; |  0 |  1  |  1 |  0  |       0      |        1      |
;; |  0 |  1  |  0 |  1  |       1      |        0      |
;; ------------------------------------------------------
;;

;;
;; Define the or-gate:
;;
(define (or-gate i1 i2 output)
  (let ((not-i1 (make-wire))
	(not-i2 (make-wire))
	(b (make-wire)))
    (inverter i1 not-i1)
    (inverter i2 not-i2)
    (and-gate not-i1 not-i2 b)
    (inverter b output)
    'ok))

;;
;; The delay time for this or-gate is twice the delay time for the 
;; inverter plus the delay time for the and-gate, or in other words:
;;
;;   D(or) = D(and) + 2*D(inv)
;;

;;
;; To unit test this, we first need to define logical-and and logical-not procedures:
;;
(define (logical-and i1 i2)
  ;; define error handler
  (define (error-handler)
    (error "Invalid signal" i1 i2))

  ;; check to make sure i1, i2 are either 0 or 1
  (if (and (or (= i1 0) (= i1 1))
	   (or (= i2 0) (= i2 1)))

      ;; perform logical-and
      (cond ((and (= i1 1) (= i2 1)) 1)
	    ((or (= i1 0) (= i2 0)) 0)
	    (else
	     (error-handler)))

      ;; input signals are wrong
      (error-handler)))

(logical-and 1 1)
;; ==> 1
(logical-and 1 0)
;; ==> 0
(logical-and 0 1)
;; ==> 0
(logical-and 0 0)
;; ==> 0

(logical-and 2 1)
;; ==> Invalid signal 2 1
(logical-and 1 2)
;; ==> Invalid signal 1 2
(logical-and 2 2)
;; ==> Invalid signal 2 2

(define (logical-not s)
  (cond ((= s 0) 1)
	((= s 1) 0)
	(else
	 (error "Invalid signal" s))))

(logical-not 0)
;; ==> 1
(logical-not 1)
;; ==> 0 

(logical-not 2)
;; ==> Invalid signal 2

;;
;; Next we define the and-gate and the inverter:
;;
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

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
		   (lambda ()
		     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

;;
;; Define and-gate-delay and inverter-delay for unit-testing:
;;
(define and-gate-delay 2)
;; ==> 2
(define inverter-delay 3)
;; ==> 3

;;
;; Unit test the and-gate:
;;
(define a1 (make-wire))
(define a2 (make-wire))
(define a3 (make-wire))

(and-gate a1 a2 a3)
;; ==> ok

(probe 'a3 a3)
;; ==> a3 0 New-value = 0

(get-signal a1)
;; ==> 0
(get-signal a2)
;; ==> 0
(get-signal a3)
;; ==> 0

;;
;; For an and-gate, setting one input to 1 does not change the value of the output:
;;
(set-signal! a1 1)
(propagate)
;; ==> done

(set-signal! a2 2)
(propagate)
;; ==> a3 4 New-value = 1

(get-signal a1)
;; ==> 1
(get-signal a2)
;; ==> 1
(get-signal a3)
;; ==> 1

(set-signal! a1 0)
(propagate)
;; ==> a3 6 New-value = 0

(set-signal! a2 0)
(propagate) 
;; ==> done

;;
;; Unit test the inverter, and reset the agenda:
;;
(define i1 (make-wire))
(define i2 (make-wire))

(set-current-time! the-agenda 0)
(current-time the-agenda)
;; ==> 0

(inverter i1 i2)
;; ==> ok

(probe 'i2 i2)
;; ==> i2 0 New-value = 0

;;
;; Let's examine the current signals on the wire:
;;
(get-signal i1)
;; ==> 0
(get-signal i2)
;; ==> 0

;; 
;; The current signals haven't propagate through the inverter yet, so let's propagate:
;;
(propagate)
;; ==> i2 3 New-value = 1

(get-signal i1)
;; ==> 0
(get-signal i2)
;; ==> 1

;;
;; Now we change the signal on wire 1:
;;
(set-signal! i1 1)
(propagate)
;; ==> i2 6 New-value = 0

(get-signal i1)
;; ==> 1
(get-signal i2)
;; ==> 0

(set-signal! i1 0)
(propagate)
;; ==> i2 9 New-value = 1

(get-signal i1)
;; ==> 0
(get-signal i2)
;; ==> 1

;;
;; Finally we're ready to unit test the or-gate and see what the propagation delay is:
;;
(define a (make-wire))
(define b (make-wire))
(define c (make-wire))
(define d (make-wire))
(define e (make-wire))
(define f (make-wire))

(inverter a c)
(inverter b d)
(and-gate c d e)
(inverter e f)

;;
;; Reset the agenda:
;;
(set-current-time! the-agenda 0)
(probe 'f f)
;; ==> f 0 New-value = 0

(set-signal! a 1)
(propagate)
;; ==> f 8 New-value  = 1

(set-signal! a 0)
(propagate)
;; ==> f 16 New-value = 0 

;;
;; We set the inverter delay to 3, and the and-gate delay 2, so the total
;; propagation delay of D(or) = D(and) + 2 * D(inv) is validated by this test.
;;