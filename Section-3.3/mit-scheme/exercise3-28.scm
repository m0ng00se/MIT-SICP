;
;; Exercise 3.28
;;
;; Define an or-gate as a primitive function box. Your "or-gate" constructor 
;; should be similar to "and-gate".
;;

;;
;; Import the circuit library:
;;
(load "circuit.scm")

;;
;; Define the or-gate:
;;
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
;; The logical-or procedure checks to make sure the two inputs are valid 
;; inputs (i.e., either 0 or 1). If the inputs are valid, it returns the 
;; logical-or of the two signals.
;;
(define (logical-or i1 i2)
  ;; define error handler
  (define (error-handler)
    (error "Invalid signal" i1 i2))

  ;; check to make sure i1, i2 are either 0 or 1
  (if (and (or (= i1 0) (= i1 1))
	   (or (= i2 0) (= i2 1)))

      ;; perform logical-or
      (cond ((or  (= i1 1) (= i2 1)) 1)
	    ((and (= i1 0) (= i2 0)) 0)
	    (else
	     (error-handler)))

      ;; input signals are wrong
      (error-handler)))

;;
;; Unit test:
;;
(logical-or 1 1)
;; ==> 1
(logical-or 1 0)
;; ==> 1
(logical-or 0 1)
;; ==> 1
(logical-or 0 0)
;; ==> 0

(logical-or 2 1)
;; ==> Invalid signal 2 1
(logical-or 1 2) 
;; ==> Invalid signal 1 2
(logical-or 2 2)
;; ==> Invalid signal 2 2

;;
;; First let's create some wires:
;;
(define a (make-wire))
(define b (make-wire))
(define c (make-wire))

;;
;; To beta-test this design, we need to define an or-gate-delay:
;;
(define or-gate-delay 2)
;; ==> 2

;;
;; Now hook the wires using an or-gate, and attach a probe to wire c:
;;
(or-gate a b c)
;; ==> 'ok
(probe 'c c)
;; ==> c 0 New-value = 0

;;
;; Get the current signals on the wire:
;;
(get-signal a)
;; ==> 0
(get-signal b)
;; ==> 0
(get-signal c)
;; ==> 0

;;
;; Update the signals on the wire and propagate the signal:
;;
(set-signal! a 1)
(propagate)
;; ==> c 2 New-value = 1

(get-signal a)
;; ==> 1
(get-signal b)
;; ==> 0
(get-signal c)
;; ==> 1

;;
;; Note that changing the signal on b at this point does not not 
;; modify the signal at c, so the probe does not change:
;;
(set-signal! b 1)
(propagate)
;; ==> done

(get-signal a)
;; ==> 1
(get-signal b)
;; ==> 1
(get-signal c)
;; ==> 1

;;
;; Neither does changing the value of a at this point modify the signal at c:
;;
(set-signal! a 0)
(propagate)
;; ==> done

(get-signal a)
;; ==> 0
(get-signal b)
;; ==> 1
(get-signal c)
;; ==> 1

(set-signal! b 0)
(propagate)
;; ==> c 8 New-value = 0

(get-signal a)
;; ==> 0
(get-signal b)
;; ==> 0
(get-signal c)
;; ==> 0