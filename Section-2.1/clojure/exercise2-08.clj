;;
;; Constructor and Selectors
;;
(defn make-interval [a b] (cons a (list b)))
(defn lower-bound [x] (first x))
(defn upper-bound [x] (second x))

;;
;; Arithmetic Procedures:
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
  (mul-interval [x]
                (make-interval (/ 1.0 (upper-bound y)))
                (make-interval (/ 1.0 (lower-bound y)))))

;;
;; Now let us construct the substraction procedure.
;;
;; First we will define a "neg-interval" procedure, that creates an arithmetical
;; inverse for a given interval. Then we will add this "negative" to the
;; first element of the combination, to arrive at the difference between the
;; two elements.
;;
(defn neg-interval [interval]
  (make-interval (* -1 (upper-bound interval))
                 (* -1 (lower-bound interval))))

;;
;; Do a few use cases, to verify that we are getting the negative:
;;
(neg-interval (make-interval 3 5))
;; ==> (-5 -3) 
(neg-interval (make-interval 0 3))
;; ==> (-3 0)
(neg-interval (make-interval -5 -3))
;; ==> (3 5)
(neg-interval (make-interval -3 0))
;; ==> (0 3)

;;
;; Now define the subtraction procedure:
;;
(defn sub-interval [x y]
  (let [z (neg-interval y)]
    (add-interval x z)))

;;
;; Run some use cases:
;;
(def i1 (make-interval 3 5))
;; ==> (3 5)

(sub-interval i1 i1)
;; ==> (-2 2)
