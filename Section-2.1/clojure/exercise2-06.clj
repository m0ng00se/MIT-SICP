;;
;; First define the procedures "zero" and "add-1":
;;
(def zero (fn [f] (fn [x] x)))

(defn add-1 [n]
  (fn [f] (fn [x] (f ((n f) x)))))

;;
;; To get a sense for what's happening here, let's experiment a bit with these procedures and
;; see where it gets us. Looking at the code, both procedures appear to take procedures as
;; arguments, although in the case of "zero" it isn't obvious that this means anything since
;; no matter what argument we pass to zero, it returns an identity procedure in one argument.
;;
;; "add-1" encapsulates a more complicated behavior, and here the argument "n" to "add-1" is
;; clearly a procedure in one argument.
;;
;; Let's work with the "inc" and "square" procedures, which are both procedures in one argument.
;;
(defn inc [n] (+ n 1))
(defn square [n] (* n n))

;;
;; Applying "zero" to a procedure returns a procedure in one argument, which itself is simply
;; the identity procedure:
;;
(zero inc)
;; ==> #[compound-procedure]

((zero inc) 1)
;; ==> 1
((zero inc) 5)
;; ==> 5
((zero inc) 10)
;; ==> 10
((zero inc) 100)
;; ==> 100

;;
;; Note that this is the case, regardless of which procedure we apply "zero" to:
;;
((zero square) 1)
;; ==> 1
((zero square) 5)
;; ==> 5
((zero square) 10)
;; ==> 10
((zero square) 100)
;; ==> 100

;;
;; So no matter what procedure we apply "zero" to, it returns a procedure which
;; itself doesn't really do anything. "zero" is, perhaps, an apt name for this procedure.
;;

;;
;; Applying "zero" to "add-1" will return a procedure in one argument:
;;
(add-1 zero)
;; ==> #[compound-procedure]

;;
;; Judging from the source code for "add-1", the procedure returned consumes a procedure in
;; one argument, which itself returns a third procedure in one argument. Let's try to apply
;; (add-1 zero) to one-argument procedures like "inc" and "square", and see what happens:
;;
((add-1 zero) inc)
;; ==> #[compound-procedure]

;;
;; Again a procedure in one argument is returned.
;;
;; Let's apply this procedure to various numbers:
;;
(((add-1 zero) inc) 1)
;; ==> 2
(((add-1 zero) inc) 5)
;; ==> 6
(((add-1 zero) inc) 10)
;; ==> 11
(((add-1 zero) inc) 100)
;; ==> 101

;;
;; Clearly, by "add-1"-ing to "zero", we end up applying the "inc" procedure just once.
;;
;; Let's see what happens if we apply the generated compound procedure to "square":
;;
(((add-1 zero) square) 1)
;; ==> 1
(((add-1 zero) square) 5)
;; ==> 25
(((add-1 zero) square) 10)
;; ==> 100
(((add-1 zero) square) 100)
;; ==> 10000

;;
;; If we apply "add-1" twice, it is reasonbly to presume that the resulting compound
;; procedure will apply its argument procedure twice:
;;
(((add-1 (add-1 zero)) inc) 1)
;; ==> 3
(((add-1 (add-1 zero)) inc) 5)
;; ==> 7
(((add-1 (add-1 zero)) inc) 10)
;; ==> 12
(((add-1 (add-1 zero)) inc) 100)
;; ==> 102

;;
;; The same results are obtained when applying the compound-procedure to "square", although
;; here the result is not the natural "succession of numerals" that we obtain when using
;; the "increment" procedure:
;;
(((add-1 (add-1 zero)) square) 2)
;; ==> 16
(((add-1 (add-1 zero)) square) 3)
;; ==> 81

;;
;; If we were lazy, we can do what the book instructs us NOT to do, and that is define procedures
;; "one" and "two" in terms of "add-1" and "zero":
;;
(def one (add-1 zero))
(def two (add-1 (add-1 zero)))

;;
;; We would then have:
;;
((one inc) 0)
;; ==> 1
((two inc) 0)
;; ==> 2

;;
;; But let's do instead what the book recommends that we do, and expend (add-1 zero) using
;; the substitution model, to gain a better understanding of what's happening:
;;
(add-1 zero)
(fn [f] (fn [x] (f ((zero f) x))))

;;
;; For any "f", the procedure (zero f) simply returns the identity procedure:
;;
(zero f)
((fn [g] (fn [y] y)) f)
(fn [y] y)

;;
;; So we can replace "(zero f)" with "(lambda (y) y)":
;;
(fn [f] (fn [x] (f ((fn [y] y) x))))
(fn [f] (fn [x] (f x)))

;;
;; This is as far as we can reduce the procedure, without supplying an argument procedure
;; (in one variable). Let's try applying "inc":
;;
((fn [f] (fn [x] (f x))) inc)
(fn [x] (inc x))

;;
;; So evaluation of the expression:
;;
((add-1 zero) inc)

;;
;; Yields the expression:
;;
(fn [x] (inc x))

;;
;; In other words, it results in one application of "inc"
;;

;;
;; Let's try to expand (add-1 (add-1 zero)) by the substitution model:
;;
(add-1 (add-1 zero))
(fn [f] (fn [x] (f (((add-1 zero) f) x))))
(fn [f] (fn [x] (f (((fn [g] (fn [y] (g y))) f) x))))
(fn [f] (fn [x] (f ((fn [y] (f y)) x))))
(fn [f] (fn [x] (f (f x))))

;;
;; Again, this is as far as we can take the problem, without supplying the
;; argument procedure (in one variable), but clearly, here we have a procedure
;; applies its argument procedure twice.
;;
;; Let's apply it to "inc" to verify this:
;;
((fn [f] (fn [x] (f (f x)))) inc)
(fn [x] (inc (inc x)))

;;
;; Finally, let's apply this to say, "0":
;;
((fn [x] (inc (inc x))) 0)
(inc (inc 0))
(inc 1)
2

;;
;; From what we have determined, it's quite simple to define "one" and "two" directly.
;; We simply apply the argument procedure once, or twice:
;;
(def one (fn [f] (fn [x] (f x))))
(def two (fn [f] (fn [x] (f (f x)))))

;;
;; Let's see if these procedures still work the way we anticipate, when applied to "inc":
;;
((one inc) 0)
;; ==> 1
((two inc) 0)
;; ==> 2
