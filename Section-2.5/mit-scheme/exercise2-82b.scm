;;
;; Exercise 2.82
;;

;; [WORKING]

(load "numbers.scm")

;;
;; Import the procedures we defined in Exercise 2.78:
;;
(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
	((number? datum) 'scheme-number)
	(else
	 (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
	((number? datum) datum)
	(else
	 (error "Bad tagged datum -- CONTENTS" datum))))

;;
;; The following predicate is useful for the solution to part (a):
;;

;;
;; Predicate that returns TRUE if all types in argument list can be converted to target type.
;; e.g., '(scheme-number complex) and 'complex is TRUE
;;       '(scheme-number complex) and 'scheme-number is FALSE
;;
(define (can-coerce-type-list-to-target-type type-list target-type)
  ;; Return TRUE if applying predicate to each item in list evaluates to TRUE for each item.
  (define (all-elements-evaluate-to-true? pred elems)
    (= (length elems)
       (length (filter pred elems))))
  ;; Return TRUE if all elements in type-list either:
  ;; (i)  are the target type;
  ;; (ii) can be coerced to target type;
  (all-elements-evaluate-to-true?
   (lambda (x)
     (or (equal? x target-type)
	 (get-coercion x target-type)))
   type-list))

(can-coerce-type-list-to-target-type '(scheme-number complex) 'complex)
;; ==> #t
(can-coerce-type-list-to-target-type '(scheme-number complex) 'scheme-number)
;; ==> #f

;;
;; We modify apply-generic to attempt coercion if an exact match in the operation table is missing:
;; 
(define (apply-generic op . args)
  (define (attempt-coercion type-list candidate-types)
    (if (null? candidate-types)
	(error "No coercion for these types" type-list)
	(let ((target-type (car candidate-types)))
	  (if (can-coerce-type-list-to-target-type type-list target-type)
	      (apply-generic op (map
				 (lambda (arg)
				   (let ((type (type-tag arg)))
				     (if (equal? type target-type)
					 arg
					 ((get-coercion type target-type) arg))))
				 args))
	      (attempt-coercion type-list (cdr candidate-types))))))

  ;; If we find a match in our operations table for the indicated types,
  ;; proceed as normal and apply the procedure. Otherwise, attempt coercion.
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (attempt-coercion type-tags type-tags)))))
