;;
;; [working]
;;

;;
;; keep assoc the same, and recursively induce the "lookup" and "insert!"
;;

;;
;; Table Constructor:
;;
(define (make-table)
  (let ((table (list '*table*)))
    
    ;; "assoc" procedure:
    (define (assoc key records)
      (cond ((null? records) #f)
	    ((equal? key (caar records)) (car records))
	    (else
	     (assoc key (cdr records)))))
	  
    ;; "lookup" procedure:
    (define (lookup keys)
      (define (lookup-iter local-keys local-table)
	(let ((subtable (assoc (car local-keys) (cdr local-table))))
	  (if subtable
	      (if (null? (cdr local-keys))
		  (cdr subtable)
		  (lookup-iter (cdr local-keys) subtable))
	      #f)))
      (lookup-iter keys table))

    ;; "insert!" procedure:
    (define (insert! keys value)
      (define (make-record keys)
	(if (null? (cdr keys)
		   (cons (car keys) value)
		   (list (car keys) (make-record (cdr keys))))))
      (define (insert-iter! local-keys local-table)
	(let ((subtable (assoc (car local-keys) (cdr local-table))))
	  (if subtable
	      (if (null? (cdr keys))
		  (set-cdr! subtable value)
		  (insert-iter! (cdr local-keys) subtable))
	      (set-cdr! local-table 
			(cons (make-entry local-keys)
			      (cdr local-table))))))
      (insert-iter! keys table))
   
    ;; "dispatch" procedure:
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
	    ((eq? m 'insert!) insert!)
	    (else
	     (error "Unknown message" m))))
    dispatch))

;;
;; Table Accessors:
;;
(define (lookup keys table)
  ((table 'lookup) keys))
(define (insert! keys value table)
  ((table 'insert!) keys table))

;;
;; First attempt a standard one-dimensional table:
;;

;; (use the [get] [set] as defined in the TEXT)

;;
;; Now try a two-dimensional table:
;;