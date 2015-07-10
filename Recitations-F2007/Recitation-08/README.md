Recitation 8 - Fall 2007 ([PDF](http://people.csail.mit.edu/jastr/6001/fall07/r08.pdf))
=======================================================================================

Special Forms
-------------

(a) quote - ```(quote expr)```

Returns whatever the reader built for expr.

(b) 'thing - syntactic sugar for ```(quote thing)```

Procedures
---------- 

(a) ```(eq? v1 v2)```

    Returns true if v1 and v2 are bitwise identical. "Works on" symbols, booleans and pairs, does not "work on" numbers and strings.

(b) ```(eqv? v1 v2)```

    Like ```eq?``` except it "works on" numbers as well.

(c) ```(equal? v1 v2)``` 
  
    Returns true if v1 and v2 print out the same. "Works on" almost everything.

Exercises
--------- 

(1) **Evaluation** - give the printed value, assuming x is bound to 5.

```
(a) '3
(b) 'x
(c) ''x
(d) (quote (3 4))
(e) ('+ 3 4) 
(f) (if '(= x 0) 7 8)
(g) (eq? 'x 'X)
(h) (eq? (list 1 2) (list 1 2))
(i) (equal? (list 1 2) (list 1 2))
(j) (let ((a (list 1 2))) (eq? a a))
```

***Boolean Formulas***

A boolean formula is a formula containing boolean operations and boolean variables. A boolean variable is either ```true``` or ```false```. ```and```, ```or``` and ```not``` are all boolean operations. For the purpose of this problem, ```and``` and ```or``` will be defined to take exactly two inputs.

Example formulas:

```
(not b)
(or b (not c))
(and (not a) (not c))
(not (or (not a) c))
(and (or a (not b)) (or (not a) c))
```

Some useful procedures:

```
(define (variable? exp)
  (symbol? exp))
(define (make-variable var)
  var)
(define (variable-name exp)
  exp)

(define (or? exp)
  (and (pair? exp) (eq? (car exp) 'or)))
(define (make-or exp1 exp2)
  (list 'or exp1 exp2))
(define (or-first exp)
  (cadr exp))
(define (or-second exp)
  (caddr exp))

(define (and? exp)
  (and (pair? exp) (eq? (car exp) 'and)))
(define (make-and exp1 exp2)
  (list 'and exp1 exp2))
(define (and-first exp)
  (cadr exp))
(define (and-second exp)
  (caddr exp))
```

TODO: Emacs, Clojure

