;;
;; [working] []
;;

;;
;; Deque --> Pair
;; (first pair -> front of deque)
;; (second pair -> rear of deque)
;;
;; each element in pair -> (data, (prev, next))
;;
;; make-deque
;; empty-deque?
;; front-deque?
;; rear-deque?
;; front-insert-deque! 
;; rear-insert-deque!
;; front-delete-deque!
;; rear-delete-deque!
;;

;;
;; Deque is specified as a pair:
;;
(define (make-deque)
  (cons '() '()))

(define (front-ptr-deque deque)
  (car deque))

(define (rear-ptr-deque deque)
  (cdr deque))

(define (set-front-ptr-deque! deque item)
  (set-car! deque item))

(define (set-rear-ptr-deque! deque item)
  (set-cdr! deque item))

;;
;; Accessors and Mutators for Deque Nodes:
;;
(define (make-deque-node item prev next)
  (cons item (cons prev next)))

(define (item-deque-node node)
  (car node))

(define (set-item-deque-node! node value)
  (set-car! node value)
  node)

(define (prev-deque-node node)
  (car (cdr node)))

(define (set-prev-deque-node! node value)
  (set-car! (cdr node))
  node)

(define (next-deque-node node)
  (cdr (cdr node)))

(define (set-next-deque-node! node value)
  (set-cdr! (cdr node))
  node)

;;
;; Accessor Procedures for Deque:
;;
(define (empty-deque? deque)
  (and (null? (front-ptr-deque deque))
       (null? (rear-ptr-deque deque))))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (car (front-ptr-deque deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (car (rear-ptr-deque deque))))

;;
;; Mutator Procedures for Deque:
;;
(define (front-insert-deque! deque item)
  (cond ((empty-deque? deque)
	 (let ((new-pair (cons item (cons '() '()))))
	   (set-front-ptr-deque! deque new-pair)
	   (set-rear-ptr-deque! deque rear-pair)
	   deque))
	(else
	 (let ((front-ptr (front-ptr-deque deque)))
	   (let ((new-pair (cons item (cons '() front-ptr))))
	     (set-cdr! front-ptr 
	     
	 (let ((new-pair (cons item (cons '() '())))
	       (front-ptr (front-ptr-deque deque)))
	   
	   
	 ;; TODO --> implement
	 '())))

(define (rear-insert-deque! deque item)
  (cond ((empty-deque? deque)
	 (let ((new-pair (cons item (cons '() '()))))
	   (set-front-ptr-deque! deque new-pair)
	   (set-rear-ptr-deque! deque new-pair)
	   deque))
	(else
	 ;; TODO --> implement
	 '())))

(define (front-delete-deque! deque)
  '())

(define (rear-delete-deque! deque)
  '())

;; 
;; Each of these procedures runs in constant time.
;;

;; 
;; We'll stick with the representation of the data structure
;; as a pair, where the first element in the pair is the 
;; "front pointer" to the deque, and the second element in the 
;; pair is the "rear pointer" to the deque.
;;
(define (make-deque) 
  (cons '() '()))

;;
;; Internal procedures:
;;
(define (front-ptr deque) 
  (car deque))
(define (rear-ptr deque) 
  (cdr deque))
(define (set-front-ptr! deque item) 
  (set-car! deque item))
(define (set-rear-ptr! deque item) 
  (set-cdr! deque item))

;;
;; Selectors:
;;
(define (empty-deque? deque)
  (and (null? (front-ptr deque))
       (null? (rear-ptr deque))))
(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT-DEQUE called with an empty deque" deque)
      (car (front-ptr deque))))
(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR-DEQUE called with an empty deque" deque)
      (car (rear-ptr deque))))

;; Insert Mutators
(define (front-insert-deque! deque item)
  (let ((new-pair (cons item '())))
    (cond ((empty-deque? deque)
	   (set-front-ptr! deque new-pair)
	   (set-rear-ptr! deque new-pair)
	   deque)
	  (else
	   (set-cdr! new-pair (front-ptr deque))
	   (set-front-ptr! deque new-pair)
	   deque))))
(define (rear-insert-deque! deque item)
  (let ((new-pair (cons item '())))
    (cond ((empty-deque? deque)
	   (set-front-ptr! deque new-pair)
	   (set-rear-ptr! deque new-pair)
	   deque)
	  (else
	   (set-cdr! (rear-ptr deque) new-pair)
	   (set-rear-ptr! deque new-pair)
	   deque))))

;; Delete Mutators
(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
	 (error "DELETE-FRONT-DEQUE! called with an empty queue" queue))
	((eq? (front-ptr deque) (rear-ptr deque))
	 (set-front-ptr! deque '())
	 (set-rear-ptr! deque '()))
	(else
	 (set-front-ptr! deque (cddr (front-ptr deque)))
	 (set-car! (cdr (front-ptr deque)) '()))))
(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
	 (error "DELETE-REAR-DEQUE! called with an empty queue" queue))
	((eq? (front-ptr deque) (rear-ptr deque))
	 (set-front-ptr! deque '())
	 (set-rear-ptr! deque '()))
	(else
	 (set-rear-ptr! deque (cadr (rear-ptr deque)))
	 (set-cdr! (cdr (rear-ptr deque)) '()))))

;; Print Procedure
(define (print-deque deque)
  (define (printable-deque-iter q)
    (if (null? q)
	'()
	(cons (car q)
	      (printable-deque-iter (cddr q)))))
  (newline)
  (display (printable-deque-iter (front-ptr deque))))

