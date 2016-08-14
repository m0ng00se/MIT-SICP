
(load-file "differentiation.el")

;;
;; In principle, so long as the combination operations remain "binary" (i.e., two
;; arguments), we should be able to implement this change by just changing where
;; in the list the combination symbol shows up. So, for instance, (+ 1 2) becomes
;; (1 + 2), and (* 1 2) becomes (1 * 2), and so forth.
;;

;;
;; Let's see if we can implement (a) just by changing a few constructors/selectors:
;;

;;
;; Summations:
;;
(defun sum? (x)
  (and (listp x) (eq (cadr x) '+)))
(defun addend (s)
  (car s))
(defun make-sum (a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (numberp a1) (numberp a2)) (+ a1 a2))
	(t
	 (list a1 '+ a2))))

(make-sum 0 10)
;; ==> 10
(make-sum 0 'x)
;; ==> x
(make-sum 10 0)
;; ==> 10
(make-sum 'x 0)
;; ==> x
(make-sum 10 20)
;; ==> 30
(make-sum 'x 'y)
;; ==> (x + y)
(make-sum 10 'x)
;; ==> (10 + x)
(make-sum 'x 10)
;; ==> (x + 10)

(setq v1 (make-sum 'x 'y))
(sum? v1)
;; ==> t
(addend v1)
;; ==> x
(augend v1)
;; ==> y

;;
;; Products:
;;
(defun product? (x)
  (and (listp x) (eq (cadr x) '*)))
(defun multiplier (p) (car p))
(defun make-product (m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (numberp m1) (numberp m2)) (* m1 m2))
	(t
	 (list m1 '* m2))))

(make-product 0 10)
;; ==> 0
(make-product 1 10)
;; ==> 10
(make-product 2 10)
;; ==> 20
(make-product 10 0)
;; ==> 0
(make-product 10 1)
;; ==> 10
(make-product 10 2)
;; ==> 20
(make-product 0 'x)
;; ==> 0
(make-product 1 'x)
;; ==> x
(make-product 2 'x)
;; ==> (2 * x)
(make-product 'x 0)
;; ==> 0
(make-product 'x 1)
;; ==> 1
(make-product 'x 2)
;; ==> (x * 2)
(make-product 'x 'y)
;; ==> (x * y)

(setq v2 (make-product 'x 'y))
(product? v2)
;; ==> t
(multiplier v2)
;; ==> x
(multiplicand v2)
;; ==> y

;;
;; Differences:
;;
(defun difference? (x)
  (and (listp x) (eq (cadr x) '-)))
(defun minuend (p) (car p))
(defun make-difference (s1 s2)
  (cond ((=number? s2 0) s1)
	((=number? s1 0) (make-product -1 s2))
	((and (numberp s1) (numberp s2)) (- s1 s2))
	(t
	 (list s1 '- s2))))

(make-difference 3 0)
;; ==> 3
(make-difference 0 3)
;; ==> -3
(make-difference 4 2)
;; ==> 2
(make-difference 2 4)
;; ==> -2
(make-difference 'x 0)
;; ==> x
(make-difference 0 'x)
;; ==> (-1 * x)
(make-difference 'x 1)
;; ==> (x - 1)
(make-difference 1 'x)
;; ==> (1 - x)
(make-difference 'x 'y)
;; ==> (x - y)

(setq v3 (make-difference 'x 'y))
(difference? v3)
;; ==> t
(minuend v3)
;; ==> x
(subtrahend v3)
;; ==> y

;;
;; Exponentiation:
;;
(defun exponentiation? (x)
  (and (listp x) (eq (cadr x) '**) (variable? (car x))))
(defun base (p)
  (car p))
(defun make-exponentiation (base exp-value)
  (cond ((and (numberp base) (numberp exp-value)) (expt base exp-value))
	((=number? exp-value 0) 1)
	((=number? exp-value 1) base)
	(t
	 (list base '** exp-value))))
     
(make-exponentiation 3 0)
;; ==> 1
(make-exponentiation 3 1)
;; ==> 3
(make-exponentiation 3 2)
;; ==> 9
(make-exponentiation 'x 0)
;; ==> 1
(make-exponentiation 'x 1)
;; ==> x
(make-exponentiation 'x 2)
;; ==> (x ** 2)
(make-exponentiation 3 'x)
;; ==> (3 ** x)
(make-exponentiation 'x 'x)
;; ==> (x ** x)

;;
;; That should be all we have to change.
;;
;; Let's walk through some use cases, to see if the differentiator gives the correct answer:
;;
(deriv 3 'x)
;; ==> 0
(deriv 'x 'x)
;; ==> 1
(deriv 'x 'y)
;; ==> 0
(deriv '(x + y) 'x)
;; ==> 1
(deriv '(x + y) 'y)
;; ==> 1
(deriv '(x + y) 'z)
;; ==> 0
(deriv '((2 * x) + y) 'x)
;; ==> 2
(deriv '((2 * x) + y) 'y)
;; ==> 1
(deriv '((x * y) + y) 'x)
;; ==> y
(deriv '((x * y) + y) 'y)
;; ==> (x + 1)
(deriv '(x - 1) 'x)
;; ==> 1
(deriv '(y - x) 'x)
;; ==> -1
(deriv '(x * y) 'x)
;; ==> y
(deriv '(x ** 3) 'x)
;; ==> (3 * (x ** 2))
(deriv '(x ** y) 'x)
;; ==> (y * (x ** (y - 1)))

(deriv '(x + 3) 'x)
;; ==> 1
(deriv '(x * y) 'x)
;; ==> y
(deriv '(x * x) 'x)
;; ==> (x + x)
(deriv '((x * y) + (x + 3)) 'x)
;; ==> (y + 1)
(deriv '((x * y) * (x + 3)) 'x)
;; ==> ((x * y) + (y * (x + 3)))

;;
;; Indeed, the procedeure seems to work correctly.
;;

;;
;; Now, to address part (b).
;;

;;
;; The first we need to create is the notion of precedence in mathematical operations.
;; We will only be dealing with two operators: addition (+) and multiplication (*),
;; which two are delineated, with their appropriate relative precedence in the table
;; below. We also use a "maximum" sentinel to kick-start the accumulator we'll be using
;; below.
;;
(setq *precedence-table*
      (list
       (list '+ 0)
       (list '* 1)
       (list 'MAX-OPERATOR 10000)))

;;
;; This notion of operator is merely a selector indicating whether the argument
;; operator is in our precedence symbol table. Presently we're only extending
;; this framework to the addition and multiplication. Invoking this procedure
;; with, for instance, the subtraction ('-) symbol will return false:
;;
(defun operator? (x)
  (defun operator-iter (table)
    (if(null table)
	'()
      (let ((entry (car table)))
	(if (eq x (car entry))
	    t
	  (operator-iter (cdr table))))))
  (operator-iter *precedence-table*))

(operator? '+)
;; ==> t
(operator? '*)
;; ==> t
(operator? '-)
;; ==> nil
(operator? '())
;; ==> nil

;;
;; Given an operator (either '+ or '*), this procedure returns the precedence ranking:
;;
(defun precedence (op)
  (defun precedence-iter (table)
    (if (null table)
	(error "Operator not defined -- PRECEDENCE: " op)
      (let ((entry (car table)))
	(cond ((eq op (car entry))
	       (cadr entry))
	      (t
	       (precedence-iter (cdr table)))))))
  (precedence-iter *precedence-table*))

(precedence '+)
;; ==> 0
(precedence '*)
;; ==> 1

;;
;; Given two argument operators, this procedure returns the lowest ranking operator.
;;
(defun get-lowest-operator (op1 op2)
  (let ((is-op1 (operator? op1))
	(is-op2 (operator? op2)))
    (cond ((and is-op1 (not is-op2)) op1)
	  ((and (not is-op1) is-op2) op2)
	  ((and (not is-op1) (not is-op2)) 'NAX-OPERATOR)
	  (t
	   (let ((p1 (precedence op1))
		 (p2 (precedence op2)))
	     (if (< p1 p2)
		 op1
	       op2))))))

(get-lowest-operator '+ '+)
;; ==> +
(get-lowest-operator '+ '*)
;; ==> +
(get-lowest-operator '* '+)
;; ==> +
(get-lowest-operator '* '*)
;; ==> *

(get-lowest-operator 2 '+)
;; ==> +
(get-lowest-operator '+ 2)
;; ==> +
(get-lowest-operator 2 2)
;; ==> max-operator

;;
;; Next we need to reimport our old friend "accumulate":
;;
(defun accumulate (op init seq)
  (if (null seq)
      init
    (funcall op (car seq)
	     (accumulate op init (cdr seq)))))

;;
;; We use accumulate to find the "lowest-order" operator in a particular expression:
;;
(defun lowest-operator (expression)
  (accumulate (lambda (a b)
		(if (operator? b)
		    (get-lowest-operator a b)
		  a))
	      'MAX-OPERATOR
	      expression))

;;
;; Let's run some unit tests:
;;
(lowest-operator '(x * z + y))
;; ==> +
(lowest-operator '(x * (z + 1)))
;; ==> *

;;
;; Which is correct, the first expression should be interpreted first as an addition,
;; then as a multiplication, while the second expression should be interpreted first
;; as a summation, and then as an addition.
;;

;;
;; Some more examples:
;;
(lowest-operator '(x + 3 * (x + y + 2)))
;; ==> +
(lowest-operator '(x + 3))
;; ==> +
(lowest-operator '(x * y * (x + 3)))
;; ==> *
(lowest-operator '((x * y) * (x + 3)))
;; ==> *
(lowest-operator '(x * (y * (x + 3))))
;; ==> *

;;
;; Let's now redefine the "sum?" and "product?" selectors:
;;
(defun sum? (expression)
  (eq '+ (lowest-operator expression)))
(defun product? (expression)
  (eq '* (lowest-operator expression)))

(setq t1 '(x + 3 * (x + y + 2)))
(sum? t1)
;; ==> t
(product? t1)
;; ==> nil

(setq t2 '(x + 3))
(sum? t2)
;; ==> t
(product? t2)
;; ==> nil

(setq t3 '(x * y * (x + 3)))
(sum? t3)
;; ==> nil
(product? t3)
;; ==> t

(setq t4 '(x * (y * (x + 3))))
(sum? t4)
;; ==> nil
(product? t4)
;; ==> t

(setq t5 '((x * y) * (x + 3)))
(sum? t5)
;; ==> nil
(product? t5)
;; ==> t

;;
;; Now we are ready to redefine the addition/multiplication selectors
;; to fit our new data model. First we need a selector "singleton?"
;; which indicates whether the expression in question has just one element:
;;
(defun singleton? (a)
  (if (and (listp a) (= (length a) 1))
      t
    nil))

;;
;; With this, we can redefine the "augend" and "multiplicand" procedures:
;;
(defun augend (expression)
  (let ((terms (cdr (memq '+ expression))))
    (if (singleton? terms)
	(car terms)
      terms)))

(defun multiplicand (expression)
  (let ((terms (cdr (memq '* expression))))
    (if (singleton? terms)
	(car terms)
      terms)))

;;
;; As in some other exercises, these two procedures are so congreunt to
;; one another that we can reduce the one to the other by abstracting
;; out the common functionality to a new procedure:
;;

(defun reduce-expression-1 (expression op)
  (let ((terms (cdr (memq op expression))))
    (if (singleton? terms)
	(car terms)
      terms)))
(defun augend (expression)
  (reduce-expression-1 expression '+))
(defun multiplicand (expression)
  (reduce-expression-1 expression '*))

;;
;; Let's run some unit tests:
;;
(augend '(1 + 2))
;; ==> 2
(augend '(x + 3))
;; ==> 3
(augend '(x + y + 4))
;; ==> (y + 4)
(augend '(x + y * z))
;; ==> (y * z)
(augend '(y * z + x))
;; ==> x

(multiplicand '(1 * 2))
;; ==> 2
(multiplicand '(x * 3))
;; ==> 3
(multiplicand '(x * y * 4))
;; ==> (y * 4)
(multiplicand '(x * (y + z)))
;; ==> (y + z)
(multiplicand '((y + z) * x))
;; ==> x

;;
;; In order to get the "addend", we basically have to do the same thing as "memq",
;; but get the produce to return everything BEFORE the symbol, rather than after.
;; We will call this procedure "prefix":
;;
(defun prefix (sym elems)
  (if (or (null elems) (eq sym (car elems)))
      '()
    (cons (car elems) (prefix sym (cdr elems)))))

(prefix '+ '(1 + 2 + 3))
;; ==> (1)
(prefix '+ '((x * y) + z))
;; ==> ((x * y))
(prefix '+ '(x * y * z))
;; ==> (x * y * z)

;;
;; Now we are ready to redefine "addend" and "multiplicand":
;;
(defun addend (expression)
  (let ((terms (prefix '+ expression)))
    (if (singleton? expression)
	(car terms)
      terms)))

(defun multiplier (expression)
  (let ((terms (prefix '* expression)))
    (if (singleton? expression)
	(car terms)
      terms)))

;;
;; Again, the congruency between these two procedures invites us
;; to create a second reduction along the lines of the first:
;;
(defun reduce-expression-2 (expression op)
  (let ((terms (prefix op expression)))
    (if (singleton? terms)
	(car terms)
      terms)))
(defun addend (expression)
  (reduce-expression-2 expression '+))
(defun multiplier (expression)
  (reduce-expression-2 expression '*))

;;
;; Let's run through some unit tests:
;;
(addend '(1 + 2))
;; ==> 1
(addend '(1 + x))
;; ==> 1
(addend '(x + 1))
;; ==> x
(addend '(x + y + z))
;; ==> x
(addend '(x + (y * z)))
;; ==> x
(addend '((x + y) + (z + u)))
;; ==> (x + y)
(addend '((y * z) + x))

;;
;; Testing multiplier:
;;
(multiplier '(1 * 2))
;; ==> 1
(multiplier '(1 * x))
;; ==> 1
(multiplier '(x * 1))
;; ==> x
(multiplier '(x * y * z))
;; ==> x
(multiplier '(x * (y + z)))
;; ==> x
(multiplier '((x + y) * (z + u)))
;; ==> (x + y)
(multiplier '((y + z) * x))
;; ==> (y + z)

;;
;; Now, finally, so long as "make-sum" and "make-product" remain defined in terms
;; of the standard infix notation that we had defined in part (a), we should be
;; OK.
;;
;; Let's try some differentiations:
;;
(deriv '(x + 3 * (x + y + 2)) 'x)
;; ==> 4
(deriv '(x * y * (x + 3)) 'x)
;; ==> ((x * y) + (y * (x + 3)))
(deriv '((x * y) * (x + 3)) 'x)
;; ==> ((x * y) + (y * (x + 3)))
(deriv '(x * (y * (x + 3))) 'x)
;; ==> ((x * y) + (y * (x + 3)))

(deriv '((x * y) + x + 3) 'x)
;; ==> (y + 1)

(deriv '((x + y) * x * 3) 'x)
;; ==> (((x + y) * 3) + (x * 3))

(deriv '(x + 1) 'x)
;; ==> 1

;;
;; And the old use cases from Exercise 2.56 (to make sure we didn't break anything):
;;
(deriv 3 'x)
;; ==> 0
(deriv 'x 'x)
;; ==> 1
(deriv 'x 'y)
;; ==> 0
(deriv '(x + y) 'x)
;; ==> 1
(deriv '(x + y) 'y)
;; ==> 1
(deriv '(x + y) 'z)
;; ==> 0
(deriv '((2 * x) + y) 'x)
;; ==> 2
(deriv '((2 * x) + y) 'y)
;; ==> 1
(deriv '((x * y) + y) 'x)
;; ==> y
(deriv '((x * y) + y) 'y)
;; ==> (x + 1)
(deriv '(x - 1) 'x)
;; ==> 1
(deriv '(1 - x) 'x)
;; ==> -1
(deriv '(y - x) 'x)
;; ==> -1
(deriv '(x * y) 'x)
;; ==> y
(deriv '(x ** 3) 'x)
;; ==> (3 * (x ** 2))
(deriv '(x ** y) 'x)
;; ==> (y * (x ** (y - 1)))

;;
;; Further references on this problem set are available at:
;;
;; http://community.schemewiki.org/?sicp-ex-2.58
;;
