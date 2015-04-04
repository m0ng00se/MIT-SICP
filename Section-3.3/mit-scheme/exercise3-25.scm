;;
;; [working]
;;

;;
;; keep assoc the same, and recursively induce the "lookup" and "insert!"
;;



(define (make-table)
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
			(cons (make-entry keys)
			      (cdr local-table))))))
      (insert!-iter keys table))

    ;; "dispatch" procedure:
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
	    ((eq? m 'insert!) insert)
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
      (define (lookup-iter key-list records)
	(let ((subtable (assoc (car key-list) (cdr records))))
	  (if subtable 
	      (if (list? subtable)
		  (lookup-iter (cdr key-list) subtable)
		  (cdr subtable))
	      #f)))
      (lookup-iter keys table))

    ;; "insert!" procedure:
    (define (insert! keys value)
      '())

    ;; "dispatch" procedure:
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
	    ((eq? m 'insert!) insert!)
	    (else
	     (error "Unknown message" m))))
    dispatch))


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

    (define (lookup keys)
      '())
    (define (insert! keys value)
      '())

    ;;
    ;; The "dispatch" procedure remains unchanged:
    ;;
    (define (dispatch m)
      (cons ((eq? m 'lookup) lookup)
	    ((eq? m 'insert!) insert!)
	    (else
	     (error "Unknown message" m))))
    dispatch))


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
			(cons (make-record keys)
			      (cdr local-table))))))

      (insert-iter! keys table)
      'ok)
   
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