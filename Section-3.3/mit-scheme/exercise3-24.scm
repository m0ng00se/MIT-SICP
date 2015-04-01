;;
;; Exercise 3.24 
;;
;; In the table implementations above, the keys are tested for equality 
;; using "equal?" (called by "assoc"). This is not always the appropriate
;; test. For instance, we might have a table with numeric keys in which 
;; we don't need an exact match to the number we're looking up, but only
;; a number within some tolerance. Design a table constructor "make-table"
;; that takes as an argument a "same-key?" procedure that will be used to 
;; test "equality" of keys. "make-table" should return a "dispatch" procedure
;; that can be used to access appropriate "lookup" and "insert!" procedures
;; for a local table. 
;;

;;
;; Table Constructors:
;;
(define (make-table same-key?)
  (let ((table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
	    ((same-key? key (caar records)) (car records))
	    (else
	     (assoc key (cdr records)))))
    (define (lookup key)
      (let ((record (assoc key (cdr table))))
	(if record
	    (cdr record)
	    #f)))
    (define (insert! key value)
      (let ((record (assoc key (cdr table))))
	(if record
	    (set-cdr! record value)
	    (set-cdr! table
		      (cons (cons key value) (cdr table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
	    ((eq? m 'insert!) insert!)
	    (else
	     (error "Unknown message" m))))
    dispatch))

;;
;; Table Accessors:
;;
(define (lookup key table)
  ((table 'lookup) key))
(define (insert! key value table)
  ((table 'insert!) key value))

;;
;; Unit Test, Symbolic Keys:
;;
(define symbol-table (make-table equal?))

(insert! 'a 10 symbol-table)
;; ==> ok
(insert! 'b 33 symbol-table)
;; ==> ok
(lookup 'a symbol-table)
;; ==> 10
(lookup 'b symbol-table)
;; ==> 33
(lookup 'c symbol-table)
;; ==> #f

;;
;; Unit Test, Numeric Keys:
;;
(define number-table (make-table =))

(insert! 1 'first number-table)
;; ==> ok
(insert! 2 'second number-table)
;; ==> ok
(lookup 1 number-table)
;; ==> first
(lookup 2 number-table)
;; ==> second
(lookup 3 number-table)
;; ==> #f