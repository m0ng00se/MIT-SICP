;;
;; Operations lookup table
;;
(load "table.scm")

;; Operations Table
(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
