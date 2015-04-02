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
    
    ;;
    ;; The "assoc" procedure remains unchanged:
    ;; 
    (define (assoc key records)
      (cond ((null? records) #f)
	    ((equal? key (caar records)) (car records))
	    (else
	     (assoc key (cdr records)))))
	  
    ;;
    ;; Implement a recursive "lookup" procedure. 
    ;;
    ;; If we can associate a subtable with the first key in the list, 
    ;; then check to see whether we are at the end of the key list. 
    ;; If yes, then we have our record; if no, then recursively invoke
    ;; the "lookup" procedure using the remainder of the key list on 
    ;; the subtable. 
    ;;
    ;; If we cannot associate a subtable with the first key in the list,
    ;; we know that there are no assoicated records.
    ;;
    (define (lookup keys)
      (define (lookup-iter local-keys local-table)
	(let ((subtable (assoc (car local-keys) (cdr local-table))))
	  (if subtable
	      (if (null? (cdr local-keys))
		  (cdr subtable)
		  (lookup-iter (cdr local-keys) subtable))
	      #f)))
      (lookup-iter keys table))

    ;;
    ;; Implement a recursive "insert!" procedure.
    ;; 
    ;; If we can associate a subtable with the first key in the list, 
    ;; then check to see whether we are at the end of the key list. 
    ;; If yes, then append the new value to the end of the subtable.
    (define (insert! keys value)

      (define (make-record keys)
	(let ((key (car keys)))
	  (if (null? (cdr keys))
	      (cons key value)
	      (list key (make-record (cdr keys))))))

      (define (insert-iter! local-keys local-table)
	(let ((subtable (assoc (car local-keys) (cdr local-table))))
	  (if subtable
	      (if (null? (cdr keys))
		  (set-cdr! subtable value)
		  (insert-iter! (cdr keys) subtable))
	      (set-cdr! local-table
			(cons (make-entry keys)
			      (cdr local-table))))))

      (insert-iter! keys table))
   
    ;;
    ;; The "dispatch" procedure remains unchanged:
    ;;
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
(define table (make-table))
(define (get keys) (lookup keys table))
(define (put keys value) (insert! keys value table))

;; (use the [get] [set] as defined in the TEXT)

;;
;; Now try a two-dimensional table:
;;