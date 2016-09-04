;;
;; Code for handling a two-dimensional table (from Chapter 3)
;;
(defun assoc (key records)
  (cond ((null records) '())
	((equal key (car (car records))) (car records))
	(t
	 (assoc key (cdr records)))))

(defun make-table ()
  (let ((local-table (list '*table*)))
    (defun lookup (key-1 key-2)
      '())
    (defun insert! (key-1 key-2 value)
      '())
    (defun dispatch (m)
      (cond ((eq m 'lookup-proc) #'lookup)
	    ((eq m 'insert-proc!) #'insert!)
	    (t
	     (error "Unknown operation -- TABLE" m))))
    #'dispatch))

(defun make-table ()
  (lexical-let ((local-table '(list *table*)))
	       (lexical-let ((lookup (lambda (key-1 key-2)
				       (let ((subtable
					      'a)))
				       (defun dispatch (m)
					 (cond ((eq m 'lookup-proc) lookup)
					       ((eq m 'insert-proc!) local-table)
					       (t
						(error "Unknown operation -- TABLE" m))))
				       #'dispatch)))
			    
;;(setq operation-table (make-table))
;;(defun get (operation-table 'lookup-proc))
;;(defun put (operation-table 'insert-proc!))
