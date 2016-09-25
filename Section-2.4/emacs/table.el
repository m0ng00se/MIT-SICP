;;
;; Code for handling a two-dimensional table (from Chapter 3)
;;

(require 'cl)

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
