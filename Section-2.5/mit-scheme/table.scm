;;
;; Code for handling a two-dimensional table (from Chapter 3)
;;

;;
;; 'assoc' procedure:
;;
(define (assoc key records)
  (cond ((null? records) false)
	((equal? key (caar records)) (car records))
	(else
	 (assoc key (cdr records)))))

(define records (list (cons 'a 1) (cons 'b 2)))
(assoc 'a records)
;; ==> (a . 1)
(assoc 'b records)
;; ==> (b . 2)
(assoc 'c records)
;; ==> #f

;;
;; 'make-table' procedure:
;;
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define records (make-table))
((records 'insert-proc!) 'a 'b 1)
((records 'lookup-proc) 'a 'b)
;; ==> 1
((records 'insert-proc!) 'a 'b 2)
((records 'lookup-proc) 'a 'b)
;; ==> 2
((records 'insert-proc!) 'a 'c 10)
((records 'insert-proc!) 'b 'a 20)
((records 'lookup-proc) 'a 'c)
;; ==> 10
((records 'lookup-proc) 'b 'a)
;; ==> 20
((records 'lookup-proc) 'b 'b)
;; ==> #f
((records 'lookup-proc) 'c 'a)
;; ==> #f
