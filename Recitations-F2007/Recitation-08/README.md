Recitation 8 - Fall 2007 ([PDF](http://people.csail.mit.edu/jastr/6001/fall07/r08.pdf))
=======================================================================================

Special Forms
-------------

(a) quote - ```(quote expr)```

    Returns whatever the reader built for expr.

(b) 'thing - syntactic sugar for ```(quote thing)```

Procedures
---------- 

(a) ```(eq? v1 v2)``` - returns true if v1 and v2 are bitwise identical. "Works on" symbols, booleans and pairs. Does not "work on" numbers and strings.

(b) ```(eqv? v1 v2)``` - like ```eq?``` except it "works on" numbers as well.

(c) ```(equal? v1 v2)``` returns true if v1 and v2 print out the same. "Works on" almost everything.



Scheme done, Emacs done.

Clojure in progress (can't update state on objects easily). 
