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

(define (front-node-deque deque)
  (car deque))

(define (rear-node-deque deque)
  (cdr deque))

(define (set-front-node-deque! deque node)
  (set-car! deque node))

(define (set-rear-node-deque! deque node)
  (set-cdr! deque node))

;;
;; Accessors and Mutators for Deque Nodes:
;;
(define (make-deque-node item prev next)
  (cons item (cons prev (cons next '()))))

(define (item-deque-node node)
  (car node))

(define (prev-deque-node node)
  (cadr node))

(define (next-deque-node node)
  (caddr node))

(define (set-item-deque-node! node item)
  (set-car! node item) node)

(define (set-prev-deque-node! node prev)
  (set-car! (cdr node) prev) node)

(define (set-next-deque-node! node next)
  (set-car! (cddr node) next) node)

;;
;; Unit Test Deque Node API:
;;
(define node (make-deque-node 'a 'b 'c))
;; ==> (a b c)
(item-deque-node node)
;; ==> a
(prev-deque-node node)
;; ==> b
(next-deque-node node)
;; ==> c

(set-item-deque-node! node 'x)
;; ==> (x b c)
(item-deque-node node)
;; ==> x
(set-prev-deque-node! node 'y)
;; ==> (x y c)
(prev-deque-node node)
;; ==> y
(set-next-deque-node! node 'z)
;; ==> (x y z)
(next-deque-node node)
;; ==> z

;;
;; Accessor Procedures for Deque:
;;
(define (empty-deque? deque)
  (and (null? (front-node-deque deque))
       (null? (rear-node-deque deque))))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (item-deque-node (front-node-deque deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (item-deque-node (rear-node-deque deque))))

;;
;; Mutator Procedures for Deque:
;;
(define (front-insert-deque! deque item)
  (cond ((empty-deque? deque)
	 (let ((new-node (make-deque-node item '() '())))
	   (set-front-node-deque! deque new-node)
	   (set-rear-node-deque! deque new-node)
	   deque))
	(else
	 (let ((first-node (front-node-deque deque)))
	   (let ((new-node (make-deque-node item '() first-node)))
	     ;;(set-prev-deque-node! first-node new-node)
	     ;; (still working)
	     (set-front-node-deque! deque new-node)
	     deque)))))

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
;; We can also define a print procedure, we runs in linear time:
;;
(define (print-deque deque)
  (define (print-deque-node node)
    (let ((current-item (item-deque-node node)))
      (display current-item)
      (display " ")
      (let ((next-node (next-deque-node node)))
	(cond ((null? next-node)
	       (newline))
	      (else
	       (print-deque-node next-node))))))
  (print-deque-node (front-node-deque deque)))

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

