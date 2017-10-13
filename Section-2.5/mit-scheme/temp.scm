;;
;; TEMP TABLE
;;
;; May be able to remove this file later.
;;

;; Operations Table
(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;; Type-Raising/Coercion Table
(define (raise-integer->rational n)
  (make-rational n 1))
(define (raise-rational->scheme-number r)
  (let ((n (car r))
	(d (cdr r)))
    (make-scheme-number (exact->inexact (/ n d)))))
(define (raise-scheme-number->complex n)
  (make-complex-from-real-imag n 0))

(put 'raise '(integer) raise-integer->rational)
(put 'raise '(rational) raise-rational->scheme-number)
(put 'raise '(scheme-number) raise-scheme-number->complex)

(define (raise x) (apply-generic 'raise x))

;; Coercion Table
(define coercion-table (make-table))
(define get-coercion (coercion-table 'lookup-proc))
(define put-coercion (coercion-table 'insert-proc!))

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number 'complex scheme-number->complex)
