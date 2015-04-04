;;
;; [working]
;;

;;
;; keep assoc the same, and recursively induce the "lookup" and "insert!"
;;



(define (make-table)
  ;; "assoc" procedure:
  (define (assoc key records)
    (cond ((null? records) #f)
	  ((equal? key (caar records)) (car records))
	  (else
	   (assoc key (cdr records)))))

  (let ((table (list '*table*)))
    
    ;; "lookup" procedure:
    (define (lookup keys)
      (define (lookup-iter key-list local-table)
	(let ((subtable (assoc (car key-list) (cdr local-table))))
	  (if subtable
	      (if (list? subtable)
		  (lookup-iter (cdr key-list) subtable)
		  (cdr subtable))
	      #f)))
      (lookup-iter keys table))
    
    ;; "insert!" procedure:
    (define (insert! keys value)
      (define (make-record key-list)
	(if (null? (cdr key-list))
	    (cons (car key-list) value)
	    (list (car key-list) (make-record (cdr key-list)))))	    
      (define (insert!-iter key-list local-table)
	(let ((subtable (assoc (car key-list) (cdr local-table))))
	  (if subtable
	      (if (list? subtable)
		  (insert!-iter (cdr keys) subtable)
		  (set-cdr! subtable value))
	      (set-cdr! local-table
			(cons (make-record keys)
			      (cdr local-table))))))
      (insert!-iter keys table)
      'ok)
    
    ;; "dispatch" procedure:
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
	    ((eq? m 'insert!) insert!)
	    (else
	     (error "Unknown message" m))))
    dispatch))

;;
;; Define helper methods:
;;
(define (lookup keys table)
  ((table 'lookup) keys))
(define (insert! keys value table)
  ((table 'insert!) keys value))

;;
;; Define specifics for "operation-table":
;;
(define operation-table (make-table))
(define get (lambda (keys) (lookup keys operation-table)))
(define put (lambda (keys value) (insert! keys value operation-table)))

;;
;; Run some unit tests:
;;

