;;
;; Exercise 3.23
;;
;; A "deque" (double-ended queue) is a sequence in which items can be 
;; inserted and deleted at either the front or the rear. Operations on
;; deques are the constructor "make-deque", the predicate "empty-deque?",
;; selectors "front-deque" and "rear-deque" and mutators "front-insert-deque!",
;; "rear-insert-deque!", "front-delete-deque!" and "rear-delete-deque!". 
;; Show how to represent deques using pairs, and give implementations of the 
;; operations. All operations should be accomplished in O(1) steps.
;;

;;
;; Conceptually, we can maintain many of the same procedures and 
;; data structures that were used with standard queues. The biggest
;; difference arises in how the mutators are defined.
;;

;;
;; Pair Representation of Deque:
;;
(define (make-deque)
  (cons '() '()))

(define (front-ptr deque)
  (car deque))

(define (rear-ptr deque)
  (cdr deque))

(define (set-front-ptr! deque item)
  (set-car! deque item))

(define (set-rear-ptr! deque item)
  (set-cdr! deque item))

;;
;; Deque Accessors:
;;
(define (empty-deque? deque)
  (null? (front-ptr deque)))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT-DEQUE called with an empty deque" deque)
      (car (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR-DEQUE called with an empty deque" deque)
      (car (rear-ptr deque))))

;;
;; Deque Mutators:
;;
(define (insert-front-deque! deque item)
  (let ((new-pair (cons item '())))
    (cond ((empty-deque? deque)
	   (set-front-ptr! deque new-pair)
	   (set-rear-ptr! deque new-pair)
	   deque)
	  (else
	   (set-front-ptr! deque (cons item (front-ptr deque)))
	   deque))))

(define (insert-rear-deque! deque item)
  (let ((new-pair (cons item '())))
    (cond ((empty-deque? deque)
	   (set-front-ptr! deque new-pair)
	   (set-rear-ptr! deque new-pair)
	   deque)
	  (else
	   (set-cdr! (rear-ptr deque) new-pair)
	   (set-rear-ptr! deque new-pair)
	   deque))))

(define (delete-front-deque! deque)
  (cond ((empty-deque? deque)
	 (error "DELETE-FRONT-DEQUE! called with an empty deque" deque))
	(else
	 (set-front-ptr! deque (cdr (front-ptr deque)))
	 deque)))

(define (delete-rear-deque! deque)
  (define (delete-rear-deque-iter! deque lst)
    (cond ((null? (cddr lst))
	   (set-cdr! lst '())
	   (set-rear-ptr! deque lst)
	   deque)
	  (else
	   (delete-rear-deque-iter! deque (cdr lst)))))
  (cond ((empty-deque? deque)
	 (error "DELETE-REAR-DEQUE! called with an empty deque" deque))
	((null? (cdr (front-ptr deque)))
	 (set-front-ptr! deque '())
	 deque)
	(else
	 (delete-rear-deque-iter! deque (front-ptr deque)))))

;;
;; Print Procedure:
;;
(define (print-deque deque)
  (car deque))

;;
;; All of these procedures run in constant time.
;;

;;
;; Unit Tests:
;;
(define deque (make-deque))
;; ==> (())
(empty-deque? deque)
;; ==> #t
(front-ptr deque)
;; ==> ()
(rear-ptr deque)
;; ==> ()

(insert-front-deque! deque 'a)
;; ==> ((a) a)
(empty-deque? deque)
;; ==> #f
(front-ptr deque)
;; ==> (a)
(rear-ptr deque)
;; ==> (a)
(front-deque deque)
;; ==> a
(rear-deque deque)
;; ==> a

(delete-front-deque! deque)
;; ==> (() a)
(empty-deque? deque)
;; ==> #t
(front-ptr deque)
;; ==> ()
(rear-ptr deque)
;; ==> (a) 
(front-deque deque)
;; ==> FRONT-DEQUE called with an empty deque (() a)
(rear-deque deque)
;; ==> REAR-DEQUE called with an empty deque (() a)

(insert-rear-deque! deque 'b)
;; ==> ((b) b)
(empty-deque? deque)
;; ==> #f
(front-ptr deque)
;; ==> (b)
(rear-ptr deque)
;; ==> (b)
(front-deque deque)
;; ==> b
(rear-deque deque)
;; ==> b

(delete-rear-deque! deque)
;; ==> (() b)
(empty-deque? deque)
;; ==> #t
(front-ptr deque)
;; ==> ()
(rear-ptr deque)
;; ==> (b)
(front-deque deque)
;; ==> FRONT-DEQUE called with an empty deque (() b)
(rear-deque deque)
;; ==> REAR-DEQUE called with an empty deque (() b)

(insert-front-deque! deque 'a)
;; ==> ((a) a) 
(delete-rear-deque! deque)
;; ==> (() a)
(insert-rear-deque! deque 'b)
;; ==> ((b) b)
(delete-front-deque! deque)
;; ==> (() b)

(insert-front-deque! deque 'a)
;; ==> ((a) a)
(insert-front-deque! deque 'b)
;; ==> ((b a) a)
(insert-front-deque! deque 'c)
;; ==> ((c b a) a)
(front-deque deque)
;; ==> c
(rear-deque deque)
;; ==> a
(insert-rear-deque! deque 'd)
;; ==> ((c b a d) d)
(insert-rear-deque! deque 'e)
;; ==> ((c b a d e) e)
(front-deque deque)
;; ==> c
(rear-deque deque)
;; ==> e
(delete-front-deque! deque)
;; ==> ((b a d e) e)
(delete-rear-deque! deque)
;; ==> ((b a d) d)
(delete-front-deque! deque)
;; ==> ((a d) d)
(delete-front-deque! deque)
;; ==> ((d) d)
(delete-front-deque! deque)
;; ==> (() d)
(insert-front-deque! deque 'a)
;; ==> ((a) a)
(insert-front-deque! deque 'b)
;; ==> ((b a) a)
(insert-front-deque! deque 'c)
;; ==> ((c b a) a)
(delete-rear-deque! deque)
;; ==> ((c b) b)
(delete-rear-deque! deque)
;; ==> ((c) c)
(delete-rear-deque! deque)
;; ==> (() c)
(empty-deque? deque)
;; ==> #t