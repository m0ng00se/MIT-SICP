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


TODO: Clojure

