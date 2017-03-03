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

;; Predicate that returns TRUE if all types in argument list can be converted to target type.
;; e.g., '(scheme-number complex) and 'complex is TRUE
;;       '(scheme-number complex) and 'scheme-number is FALSE
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
;; It's also useful to have a predicate that tests whether the type for two argument sets are identical:
;;

;; TRUE if both argument lists consists of identical types.
(define (type-list-identical? arglist1 arglist2)
  (let ((type-tags-1 (map type-tag arglist1))
	(type-tags-2 (map type-tag arglist2)))
    (equal? type-tags-1 type-tags-2)))

(define c1 (make-complex-from-real-imag 1 1))

(type-list-identical? (list 1 1) (list 1 1))
;; ==> #t
(type-list-identical? (list 1 c1) (list 1 c1))
;; ==> #t
(type-list-identical? (list c1 c1) (list c1 c1))
;; ==> #t
(type-list-identical? (list c1 1) (list c1 1))
;; ==> #t

(type-list-identical? (list 1 1) (list 1 c1))
;; ==> #f
(type-list-identical? (list 1 1) (list c1 c1))
;; ==> #f
(type-list-identical? (list 1 1) (list c1 1))
;; ==> #f

(type-list-identical? (list 1 c1) (list 1 1))
;; ==> #f
(type-list-identical? (list 1 c1) (list c1 c1))
;; ==> #f
(type-list-identical? (list 1 c1) (list c1 1))
;; ==> #f

(type-list-identical? (list c1 c1) (list 1 1))
;; ==> #f
(type-list-identical? (list c1 c1) (list 1 c1))
;; ==> #f
(type-list-identical? (list c1 c1) (list c1 1))
;; ==> #f

(type-list-identical? (list c1 1) (list 1 1))
;; ==> #f
(type-list-identical? (list c1 1) (list 1 c1))
;; ==> #f
(type-list-identical? (list c1 1) (list c1 c1))
;; ==> #f

;;
;; We modify apply-generic to attempt coercion if an exact match in the operation table is missing,
;; and also bring bring in the auxilliary procedures we've defined:
;; 
(define (apply-generic op . args)
  ;; Imported procedures:
  (define (can-coerce-type-list-to-target-type type-list target-type)
    (define (all-elements-evaluate-to-true? pred elems)
      (= (length elems)
	 (length (filter pred elems))))
    (all-elements-evaluate-to-true?
     (lambda (x)
       (or (equal? x target-type)
	   (get-coercion x target-type)))
     type-list))
  (define (type-list-identical? arglist1 arglist2)
    (let ((type-tags-1 (map type-tag arglist1))
	  (type-tags-2 (map type-tag arglist2)))
      (equal? type-tags-1 type-tags-2)))
  
  ;; For each element in candidate-types, see whether the entire
  ;; type-list can be coerced to that type. If so, then re-invoke
  ;; apply-generic, coercing arguments as appropraite. Otherwise,
  ;; continue walking down the list of candidate types.
  (define (attempt-coercion type-list candidate-types)
    (if (null? candidate-types)
	(error "No method for these types -- APPLY-GENERIC" (list op type-list))
	(let ((target-type (car candidate-types)))
	  (if (can-coerce-type-list-to-target-type type-list target-type)
	      (let ((new-args (map (lambda (arg)
				     (let ((type (type-tag arg)))
				       (if (equal? type target-type)
					   arg
					   ((get-coercion type target-type) arg))))
				   args)))
		;; if-block prevents infinite loop if the operation is genuinely missing from table
		(if (type-list-identical? args new-args)
		    (attempt-coercion type-list (cdr candidate-types))
		    (apply apply-generic op new-args)))
	      (attempt-coercion type-list (cdr candidate-types))))))
  
  ;; If we find a match in our operations table for the indicated types,
  ;; proceed as normal and apply the procedure. Otherwise, attempt coercion.
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (attempt-coercion type-tags type-tags)))))

;;
;; Unit-testing apply-generic directly:
;;
(apply-generic 'add 1 1)
;; ==> 2
(apply-generic 'add 1 c1)
;; ==> (complex rectangular 2 . 1)
(apply-generic 'add c1 c1)
;; ==> (complex rectangular 2 . 2)
(apply-generic 'add c1 1)
;; ==> (complex rectangular 2 . 1)

;;
;; Unit-testing through the generic procedure:
;;
(add 1 1)
;; ==> 2
(add 1 c1)
;; ==> (complex rectangular 2 . 1)
(add c1 c1)
;; ==> (complex rectangular 2 . 2)
(add c1 1)
;; ==> (complex rectangular 2 . 1)

;;
;; We can add coercion procedures for rational numbers as well:
;;
(define (rational->scheme-number r)
  (let ((number (contents r)))
    (/ (car number) (cdr number))))
(define (rational->complex r)
  (let ((number (rational->scheme-number r)))
    (make-complex-from-real-imag number 0)))

(put-coercion 'rational 'scheme-number rational->scheme-number)
(put-coercion 'rational 'complex rational->complex)

(add 1 r1)
;; ==> 3/2
(add r1 1)
;; ==> 3/2
(add r1 c1)
;; ==> (complex rectangular 3/2 . 1)
(add c1 r1)
;; ==> (complex rectangular 3/2 . 1)

;;
;; [WORKING] Modify this to work with an add-n procedure
;;

;;
;; For part (b), this coercion stragey only works if all the arguments to be coerced
;; are in the same branch of the type tree/hierarhcy. In other words, using the geometrical
;; type tree indicated in Figure 2.26 in the text, there is no way to coerce a "right triangle"
;; into a "parallelogram" (since, obviously, the right triangle has 3 sides and the parallelogram
;; has 4 sides).
;;

;;
;; Let's run some unit tests to prove this.
;;
;; First define and install a polygon package (objects are no-ops):
;;
(define (install-polygon-package)
  (define (tag x) (attach-tag 'polygon x))
  (put 'add-area '(polygon polygon)
       (lambda (x y) (tag 'area-of-polygon)))
  (put 'make 'polygon
       (lambda () (tag 'polygon)))
  (put 'make 'triangle
       (lambda () (tag 'triangle)))
  (put 'make 'right-triangle
       (lambda () (tag 'right-triangle)))

  (put 'make 'quadrilateral
       (lambda () (tag 'quadrilateral)))
  'done)
(install-polygon-package)

;; Constructors for the polygon package:
(define (make-polygon)
  ((get 'make 'polygon)))
(define (make-triangle)
  ((get 'make 'triangle)))
(define (make-right-triangle)
  ((get 'make 'right-triangle)))
(define (make-quadrilateral)
  ((get 'make 'quadrilateral)))

;; Define a method which takes two generic polygon arguments (method is no-op):
(define (add-area p1 p2) (apply-generic 'add-area p1 p2))

;; Next define the coercion hierarchy (coercion methods are no-ops):
(define (triangle->polygon arg)
  (make-polygon))
(define (right-triangle->triangle arg)
  (make-triangle))
(define (quadrilateral->polygon arg)
  (make-polygon))




