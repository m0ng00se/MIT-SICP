;;
;; General operations that can be used and defined
;; on complex numbers, independent of the data
;; representation used to define them:
;;
(require 'cl)

(defun add-complex (z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
		       (+ (imag-part z1) (imag-part z2))))

(defun sub-complex (z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
		       (- (imag-part z1) (imag-part z2))))

(defun mul-complex (z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
		     (+ (angle z1) (angle z2))))

(defun div-complex (z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
		     (- (angle z1) (angle z2))))

;;
;; Let's define the "table" that we will use for "put/get":
;;
;; (This is from peeking ahead into Section 3 of the text)
;;
(defun assoc (key records)
  (cond ((null records) '())
	((equal key (car (car records))) (car records))
	(t
	 (assoc key (cdr records)))))

(defun make-table ()
  (lexical-let* ((local-table (list '*table*)))
		(defun lookup (key1 key2)
		  (lexical-let* ((subtable (assoc key1 (cdr local-table))))
				(if subtable
				    (lexical-let* ((record (assoc key2 (cdr subtable))))
						  (if record
						      (cdr record)
						    '()))
				  '())))
		(defun insert (key1 key2 value)
		  (lexical-let* ((subtable (assoc key1 (cdr local-table))))
				(if subtable
				    (lexical-let* ((record (assoc key2 (cdr subtable))))
						  (if record
						      (setcdr record value)
						    (setcdr subtable
							    (cons (cons key2 value)
								  (cdr subtable)))))
				  (setcdr local-table
					  (cons (list key1
						      (cons key2 value))
						(cdr local-table))))))
		(defun dispatch (m)
		  (cond ((eq m 'lookup-proc) #'lookup)
			((eq m 'insert-proc!) #'insert)
			(t
			 (error "Unknown operation -- TABLE" m))))
		#'dispatch))

(setq operation-table (make-table))
(setq get (funcall operation-table 'lookup-proc))
(setq put (funcall operation-table 'insert-proc!))

