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
;; For part (b), there are at least two cases where this coercion strategy would not work.
;;
;; For case (i), if the procedure expects arguments which are on a different branch in the
;; hierarchy tree from the arguments that have been supplied, no coercion can take place.
;; Suppose we have type A, and two subtypes where B < A and C < A are subtypes of A, but
;; B and C are not subtypes of one another.
;;
;; In this case, if the procedure expects arguments of type C, there is no way we can
;; supply arguments of type B and successfully coerce the arguments into type C.
;;
;; For case (ii), suppose that we have a procedure that takes two arguments of type A, and
;; suppose we invoke this procedure with two arguments of type B, where B < A is a subtype
;; A and B can be coerced to A. In this case, our strategy will still fail, since the
;; algorithm will attempt to coerce B to B, which will fail, and then it will again attempt
;; to coerce B to B, which again will fail, even though in principle B should be coerce-able
;; to type A.
;;
;; Case (ii) is probably a more important case to be aware of than case (i).
;;

;;
;; Let's construct a polygon package, mirroring the hierarchy defined in the text, to test
;; these ideas out. All the objects/methods in this package are no-ops:
;;
(define (install-polygon-package)
  (define (tag x) (attach-tag x 'shape))
  
  ;; Generic Methods
  (put 'add-area '(polygon polygon)
       (lambda (x y) (cons 'polygon 'combined-area-of-polygon)))
  (put 'add-area-triangle '(triangle triangle)
       (lambda (x y) (cons 'triangle 'combined-area-of-triangles)))
  (put 'add-area-quadrilateral '(quadrilateral quadrilateral)
       (lambda (x y) (cons 'quadrilateral 'combined-area-of-quadrilaterals)))

  ;; Polygon Constructor
  (put 'make 'polygon
       (lambda () (tag 'polygon)))
  ;; Triangle Branch
  (put 'make 'triangle
       (lambda () (tag 'triangle)))
  (put 'make 'right-triangle
       (lambda () (tag 'right-triangle)))
  (put 'make 'isosceles-triangle
       (lambda () (tag 'isosceles-triangle)))
  (put 'make 'isosceles-right-triangle
       (lambda () (tag 'isosceles-right-triangle)))
  (put 'make 'equilateral-triangle
       (lambda () (tag 'equilateral-triangle)))
  ;; Quadrilateral Branch
  (put 'make 'quadrilateral
       (lambda () (tag 'quadrilateral)))
  (put 'make 'trapezoid
       (lambda () (tag 'trapezoid)))
  (put 'make 'kite
       (lambda () (tag 'kite)))
  (put 'make 'parallelogram
       (lambda () (tag 'parallelogram)))
  (put 'make 'rhombus
       (lambda () (tag 'rhombus)))
  (put 'make 'rectangle
       (lambda () (tag 'rectangle)))
  (put 'make 'square
       (lambda () (tag 'square)))
     
  'done)
(install-polygon-package)

;; Constructors for the polygon package:
(define (make-polygon)
  ((get 'make 'polygon)))
(define (make-triangle)
  ((get 'make 'triangle)))
(define (make-right-triangle)
  ((get 'make 'right-triangle)))
(define (make-isosceles-triangle)
  ((get 'make 'isosceles-triangle)))
(define (make-isosceles-right-triangle)
  ((get 'make 'isosceles-right-triangle)))
(define (make-equilateral-triangle)
  ((get 'make 'equilateral-triangle)))
(define (make-quadrilateral)
  ((get 'make 'quadrilateral)))
(define (make-trapezoid)
  ((get 'make 'trapezoid)))
(define (make-kite)
  ((get 'make 'kite)))
(define (make-parallelogram)
  ((get 'make 'parallelogram)))
(define (make-rhombus)
  ((get 'make 'rhombus)))
(define (make-rectangle)
  ((get 'make 'rectangle)))
(define (make-square)
  ((get 'make 'square)))

;; Define methods which take two generic polygon arguments (methods are no-ops):
(define (add-area p1 p2) (apply-generic 'add-area p1 p2))
(define (add-area-triangle t1 t2) (apply-generic 'add-area-triangle t1 t2))
(define (add-area-quadrilateral q1 q2) (apply-generic 'add-area-quadrilateral q1 q2))

;; Let's construct some objects:
(define p1 (make-polygon))
(define p2 (make-polygon))
(define t1 (make-triangle))
(define t2 (make-triangle))
(define q1 (make-quadrilateral))
(define q2 (make-quadrilateral))

;; We can add two polygons together:
(add-area p1 p2)
;; ==> (polygon . combined-area-of-polygon)

;; And we can add two triangles together using the specifically typed method:
(add-area-triangle t1 t1)
;; ==> (triangle . combined-area-of-triangles)

;; But we cannot use triangles as arguments in the method defined for polygons,
;; and we cannot combined triangle and polygon arguments in the same method,
;; even though triangles are a type of polgyon:
(add-area t1 t2)
;; ==> No method for these types -- APPLY-GENERIC (add-area (triangle triangle))
(add-area p1 t1)
;; ==> No method for these types -- APPLY-GENERIC (add-area (polygon triangle))

;; The reason of course is b/c we haven't defined the hierarchy yet using
;; coercion methods. Let's do that right now:
(define (triangle->polygon arg)
  (make-polygon))
(define (right-triangle->triangle arg)
  (make-triangle))
(define (isosceles-triangle->triangle arg)
  (make-triangle))
(define (isosceles-right-triangle->right-triangle arg)
  (make-right-triangle))
(define (isosceles-right-triangle->isosceles-triangle arg)
  (make-isosceles-triangle))
(define (equilateral-triangle->isosceles-triangle arg)
  (make-isosceles-triangle))
(define (quadrilateral->polygon arg)
  (make-polygon))
(define (kite->quadrilateral arg)
  (make-quadrilateral))
(define (trapezoid->quadrilateral arg)
  (make-quadrilateral))
(define (parallelogram->trapezoid arg)
  (make-trapezoid))
(define (rhombus->kite arg)
  (make-kite))
(define (rhombus->parallelogram arg)
  (make-parallelogram))
(define (rectangle->parallelogram arg)
  (make-parallelogram))
(define (square->rectangle arg)
  (make-rectangle))
(define (square->rhombus arg)
  (make-rhombus))

(put-coercion 'triangle 'polygon triangle->polygon)
(put-coercion 'right-triangle 'triangle right-triangle->triangle)
(put-coercion 'isosceles-triangle 'triangle isosceles-triangle->triangle)
(put-coercion 'isosceles-right-triangle 'right-triangle isosceles-triangle->triangle)
(put-coercion 'isosceles-right-triangle 'isosceles-triangle isosceles-right-triangle->isosceles-triangle)
(put-coercion 'equilateral-triangle 'isosceles-triangle equilateral-triangle->isosceles-triangle)
(put-coercion 'quadrilateral 'polygon quadrilateral->polygon)
(put-coercion 'kite 'quadrilateral kite->quadrilateral)
(put-coercion 'trapezoid 'quadrilteral trapezoid->quadrilateral)
(put-coercion 'parallelogram 'trapezoid parallelogram->trapezoid)
(put-coercion 'rhombus 'kite rhombus->kite)
(put-coercion 'rhombus 'parallelogram rhombus->parallelogram)
(put-coercion 'rectangle 'parallelogram rectangle->parallelogram)
(put-coercion 'square 'rectangle square->rectangle)
(put-coercion 'square 'rhombus square->rhombus)

;;
;; Now that we have our coercion procedures defined, let's see how our
;; coercion strategy works for the two cases identified above.
;;


 
;; [WORKING]
