;;
;; Exercise 3.22
;;
;; Instead of representing a queue as a pair of pointers, we can build
;; a queue as a procedure with local state. The local state will consist
;; of pointers to the beginning and the end of an ordinary list. Thus, 
;; the "make-queue" procedure will have the form
;;
;;  (define (make-queue)
;;    (let ((front-ptr ...)
;;          (rear-ptr ...))
;;      <definitions of internal procedures>
;;     (define (dispatch m) ...){
;;     dispatch))
;;
;; Complete the definition of "make-queue" and provide implementations 
;; of the queue operations using this representation.
;;

;;
;; Define constructor:
;;
(define (make-queue)
  ;; Front and Rear Pointers
  (let ((front-ptr '())
	(rear-ptr '()))

    ;; Internal Procedures
    (define (empty-queue?)
      (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
	  (error "FRONT called with an empty queue")
	  (car front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
	(cond ((empty-queue?)
	       (set! front-ptr new-pair)
	       (set! rear-ptr new-pair)
	       front-ptr)
	      (else
	       (set-cdr! rear-ptr new-pair)
	       (set! rear-ptr new-pair)
	       front-ptr))))
    (define (delete-queue!)
      (cond ((empty-queue?) 
	     (error "DELETE! called with an empty queue"))
	    (else
	     (set! front-ptr (cdr front-ptr))
	     front-ptr)))
    (define (print-queue)
      (newline)
      (display front-ptr))

    ;; Dispatch Procedure
    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) (empty-queue?))
	    ((eq? m 'front-queue) (front-queue))
	    ((eq? m 'insert-queue!) insert-queue!)
	    ((eq? m 'delete-queue!) (delete-queue!))
	    ((eq? m 'print-queue) (print-queue))
	    (else
	     (error "QUEUE - Unknown message" m))))
    dispatch))

;;
;; Define selectors and mutators:
;;
(define (empty-queue? queue)
  (queue 'empty-queue?))
(define (front-queue queue)
  (queue 'front-queue))
(define (insert-queue! queue item)
  ((queue 'insert-queue!) item))
(define (delete-queue! queue)
  (queue 'delete-queue!))
(define (print-queue queue)
  (queue 'print-queue))

;;
;; Unit Tests
;;
(define q (make-queue))
;; ==> [compiled-procedure]
(empty-queue? q)
;; ==> #t
(insert-queue! q 'a)
;; ==> (a)
(empty-queue? q)
;; ==> #f
(insert-queue! q 'b)
;; ==> (a b)
(insert-queue! q 'c)
;; ==> (a b c)
(print-queue q)
;; ==> (a b c)
(delete-queue! q)
;; ==> (b c)
(delete-queue! q)
;; ==> (c)
(insert-queue! q 'd)
;; ==> (c d)
(delete-queue! q)
;; ==> (d)
(delete-queue! q)
;; ==> ()
(empty-queue? q)
;; ==> #t