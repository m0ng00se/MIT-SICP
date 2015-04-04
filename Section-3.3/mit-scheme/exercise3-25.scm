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
	      (cond ((list? subtable) (lookup-iter (cdr key-list) subtable))
		    ((null? (cdr key-list)) (cdr subtable))
		    (else #f))
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
	      subtable
	      (set-cdr! local-table
			(cons (make-record key-list)
			      (cdr local-table))))))
      (insert!-iter keys table)
      'ok)
    
    ;; "print" procedure (for debugging):
    (define (print)
      table)

    ;; "dispatch" procedure:
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
	    ((eq? m 'insert!) insert!)
	    ((eq? m 'print) print)
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
(define (print table)
  ((table 'print)))

;;
;; Define specifics for "operation-table":
;;
(define operation-table (make-table))
(define get (lambda (keys) (lookup keys operation-table)))
(define put (lambda (keys value) (insert! keys value operation-table)))

;;
;; Run some unit tests:
;;
(get '(a))
;; ==> #f
(put '(a) 1)
;; ==> ok
(get '(a))
;; ==> 1
(put '(a) 2)
;; ==> ok
(get '(a))
;; ==> 2
(put '(b) 33)
;; ==> ok
(get '(a))
;; ==> 2
(get '(b))
;; ==> 33
(get '(a b c d))
;; ==> #f
(put '(a b c d) 100)
;; THESE TEST STILL FAIL

;;
;; USE ASE YOURE WOKRING ON
;;
(get '(a))
(put '(a) 1)
(get '(a))
(get '(a b c d))
(put '(a b c d) 100)
(get '(a b c d))

;; THESE TESTS STILL FAIL
(get '(math))
;; ==> #f
(get '(math +))
;; ==> #f

(put '(math +) 43)
;; ==> ok
(put '(math -) 45)
;; ==> ok
(put '(math *) 42)
;; ==> ok

