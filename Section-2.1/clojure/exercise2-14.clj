;;
;; Exercise 2.14
;;
;; Demonstrate that Lem is right. Investigate the behavior of the system on a variety
;; of arithmetic expressions. Make some intervals A and B, and use them in computing
;; the expressions A/A and A/B. You will get the most insight by using intervals whose width
;; is a small percentage of the center value. Examine the results of the computation in
;; center-percent form (see Exercise 2.12).
;;

;;
;; Constructors and Selectors:
;;
(defn make-interval [a b] (cons a (list b)))
(defn lower-bound [x] (first x))
(defn upper-bound [x] (second x))

;;
;; Arithmetical Procedures:
;;
(defn add-interval [x y]
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(defn mul-interval [x y]
  (let [p1 (* (lower-bound x) (lower-bound y))
        p2 (* (lower-bound x) (upper-bound y))
        p3 (* (upper-bound x) (lower-bound y))
        p4 (* (upper-bound x) (upper-bound y))]
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(defn div-interval [x y]
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;;
;; Let's add the "make-center-width" and "make-center-percent" constructors:
;;
(defn make-center-width [c w]
  (make-interval (- c w) (+ c w)))
(defn center [i]
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(defn width [i]
  (/ (- (upper-bound i) (lower-bound i)) 2))

(defn make-center-percent [c p]
  (let [w (* c p)]
    (make-center-width c w)))
(defn percent [i]
  (/ (width i) (center i)))

;;
;; Next define the two "parallel" procedures, and see whether they give inconsistent answers:
;;
(defn par1 [r1 r2]
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(defn par2 [r1 r2]
  (let [one (make-interval 1 1)]
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

;;
;; Let's define two intervals, and see what the results are:
;;
(def x (make-center-percent 1 0.1))
;; ==> (0.9 1.1)
(def y (make-center-percent 2 0.1))
;; ==> (1.8 2.2)

(par1 x y)
;; ==> (0.4909090909 0.896296296296)
(par2 x y)
;; ==> (0.6 0.73333333)

;;
;; Clearly these two numbers are off by quite a bit.
;;
;; Let's get a better idea of how far apart they are:
;;
(center (par1 x y))
;; ==> 0.6936
(center (par2 x y))
;; ==> 0.6666

;;
;; The centers are "close", but clearly do not precisely coincide.
;;
(width (par1 x y))
;; ==> 0.20269
(width (par2 x y))
;; ==> 0.0666

;;
;; The widths are very "different", again clearly indicating that these are not "similar" intervals.
;;
(percent (par1 x y))
;; ==> 0.292233
(percent (par2 x y))
;; ==> 0.1

;;
;; The percentage error using the first procedure is nearly three times the error produced using
;; the second procedure.
;;

;;
;; As an additional check, let's see that the two "parallel" operations are
;; at least symmetric, which they should be:
;;
(= (par1 x y) (par1 y x))
;; ==> true
(= (par2 x y) (par2 y x))
;; ==> true

;;
;; Let's see how the two methods fare when using values in a more "practical" context.
;; Suppose we have one resistor, rated 40 kOhm with an error of 0.5%, and a second
;; resistor rated at 65 kOhm with an error tolerance of 1.25%.
;;
(def x (make-center-percent 40000 0.005))
(def y (make-center-percent 65000 0.0125))

(par1 x y)
;; ==> (24097.747907086425 25442.12044716913)
(par2 x y)
;; ==> (24567.01526625796 24956.137247966042)

;;
;; Clearly the intervals are "off", but not by so large a margin as before.
;;
;; Let's compare the center points and percentage width:
;;
(center (par1 x y))
;; ==> 24769.934177127776
(center (par2 x y))
;; ==> 24761.576257112

;;
;; Indeed, the center points are quite close!
;;
(percent (par1 x y))
;; ==> 0.02713718434754828
(percent (par2 x y))
;; ==> 0.007857375024667879

;;
;; The percentage width is (again) about 3 times wider using the "par1" procedure than
;; when using the "par2" procedure, but both percentage widths are quite "tight".
;;

;;
;; As a final example, let's use an example where the percentage width is
;; egregiously large compared to the center value:
;;
(def x (make-center-percent 10 0.3))
;; ==> (7.0 13.0)
(def y (make-center-percent 10 0.4))
;; ==> (6.0 14.0)

;;
;; Both intervals (clearly) have the same center:
;;
(center x)
;; ==> 10.0
(center y)
;; ==> 10.0

;;
;; However, running these intervals through the two parallel resistor calculations
;; gives wildly divergent answers:
;;
(par1 x y)
;; ==> (1.5555555555555554 14.0)
(par2 x y)
;; ==> (3.230769230769231 6.7407407407407405)

;;
;; The "center" of par1 does not even lie within the bounds of par2!
;;
(center (par1 x y))
;; ==> 7.777777777777778
(center (par2 x y))
;; ==> 4.985754985754985
(> (center (par1 x y)) (upper-bound (par2 x y)))
;; ==> true

;;
;; So clearly it's true that the "same" algebraic expression can give rise to "different"
;; answers uing the interval arithmetic package. However, the answers seem to "converge"
;; when using intervals where the percentage width is a "small" fraction of the center value.
;; For these more "practical" types of intervals, the two procedures are not so far apart.
;;

;;
;; However, the problem also asks us to calculate A/A for a range of intervals.
;;
;; Let's do this, using the two "extreme" examples: one where the percentage width is a
;; tiny fraction of the center value, and one where the percentage width is an egregiously
;; large fracation of the center value.
;;
(def x (make-center-percent 40000 0.005))
(def y (make-center-percent 65000 0.0125))

(div-interval x x)
;; ==> (0.990049751243781 1.0100502512562815)
(div-interval y y)
;; ==> (0.9753086419753086 1.0253164556962027)

(center (div-interval x x))
;; ==> 1.0000500012500313
(center (div-interval y y))
;; ==> 1.0003125488357556

(percent (div-interval x x))
;; ==> 0.009999750006249898
(percent (div-interval y y))
;; ==> 0.024996094360256276

;;
;; Clearly these values are constrained quite tightly around 1.00, which is what we
;; would expect for a "properly functioning" interval arithmetic library.
;;

;;
;; Moreover, as 40000 / 65000 = 0.61538 and 65000 / 40000 = 1.625, we would expect
;; the respective intervals to "multiply out" to something similar as well:
;;
(center (div-interval x y))
;; ==> 0.6155192518061716
(center (div-interval y x))
;; ==> 1.6251421910547763

;;
;; and indeed, that is the (roughly) result we obtain (to within an excellent tolerance).
;;

;;
;; Now let's try the same exercise with the more "widely divergent" intervals:
;;
(def x (make-center-percent 10 0.3))
(def y (make-center-percent 10 0.4))

(center (div-interval x x))
;; ==> 1.1978021978021978
(center (div-interval y y))
;; ==> 1.3809523809523807

(percent (div-interval x x))
;; ==> 0.5504587155963302
(percent (div-interval y y))
;; ==> 0.6896551724137931
       
;;
;; Let's try to compose the two intervals, in each case we would anticipate getting 1:
;;
(center (div-interval x y))
;; ==> 1.3333333333333333
(center (div-interval y x))
;; ==> 1.2307692307692308

(percent (div-interval x y))
;; ==> 0.625
(percent (div-interval y x))
;; ==> 0.6249999999999999

;;
;; Clearly, in all these cases, the values we obtain are (quite) far from what we
;; would expect, were the interval arithmetic library functioning properly.
;;

;;
;; The problem statement indicates that "you will get the most insight by using intervals
;; whose width is a small percentage of the center value." I am not able to reproduce these
;; results. Rather, the widest divergence appears to occur when the percentage width is
;; very large in relation to the center value.
;;
