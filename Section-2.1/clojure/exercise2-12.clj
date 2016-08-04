;;
;; Exercise 2.12
;;
;; Define a constructor "make-center-percent" that takes a center and a percentage
;; and produces the desired interval. You must also define a selector "percent"
;; that produces the percentage tolerance for a given interval. The "center" selector
;; is the same as the one shown above.
;;

;;
;; First define the original constructor and selectors:
;;
(defn make-interval [a b]
  (cond (< a b) (cons a (list b))
        :else
        (println "error constructing interval!")))
(defn lower-bound [x] (first x))
(defn upper-bound [x] (second x))

;;
;; Define the "additive tolerance" constructors from the text:
;;
(defn make-center-width [c w]
  (make-interval (- c w) (+ c w)))
(defn center [i]
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(defn width [i]
  (/ (- (upper-bound i) (lower-bound i)) 2))

;;
;; A use case given in the text is: 3.5 += 0.15, resulting in [3.35, 3.65].
;;
;; Let's see if our code is able to reproduce this:
;;
(def x (make-center-width 3.5 0.15))
(center x)
;; ==> 3.5
(width x)
;; ==> 0.15
(lower-bound x)
;; ==> 3.35
(upper-bound x)
;; ==> 3.65

;;
;; It looks good.
;;

;;
;; Finally answer the question:
;;
(defn make-center-percent [c p]
  (let [w (* c p)]
    (make-center-width c w)))

;;
;; The "center" selector is already given in the text.
;;
;; We define the "percent" selector:
;;
(defn percent [i]
  (/ (width i) (center i)))

;;
;; Run a use case:
;;
(def y (make-center-percent 3 0.1))
;; ==> (2.7 3.3)

(center y)
;; ==> 3.0
(percent y)
;; ==> 0.1
(width y)
;; ==> 0.3
(lower-bound y)
;; ==> 2.7
(upper-bound y)
;; ==> 3.3
