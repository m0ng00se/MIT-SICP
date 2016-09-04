;;
;; Code for handling a two-dimensional table (from Chapter 3)
;;
(defun assoc (key records)
  (cond ((null records) '())
	((equal key (car (car records)) (car records)))
	(t
	 (assoc key (cdr records)))))

(defun make-table ()
  (let ((local-table (list '*table*)))
    (defun lookup (key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record (assoc key-2 (cdr subtable))))
	      (if record
		  (cdr record)
		'()))
	  '())))
    (defun insert! (key-1 key-2 value)
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
    (defun dispatch (m)
      (cond ((eq m 'lookup-proc) lookup)
	    ((eq m 'insert-proc!) insert!)
	    (t
	     (error "Unknown operation -- TABLE" m))))
    dispatch))

(defun operation-table (make-table))
(defun get (operation-table 'lookup-proc))
(defun put (operation-table 'insert-proc!))
