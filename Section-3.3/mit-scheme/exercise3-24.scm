;;
;; [working]
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
;; Unit Test Symbols:
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
;; Unit Test Numbers:
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