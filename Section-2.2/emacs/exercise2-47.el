;;
;; Exercise 2.47
;;
;; Here are two possible constructors for frames:
;;
;;  (define (make-frame origin edge1 edge2)
;;   (list origin edge1 edge2))
;;
;;  (define (make-frame origin edge1 edge2)
;;   (cons origin (cons edge1 edge2)))
;;
;; For each constructor supply the appropriate selectors to produce an implementation for frames.
;;

;;
;; First constructor and supporting selectors:
;;
(defun make-frame (origin edge1 edg2)
  (list origin edge1 edge2))

(defun origin-frame (f)
  (car f))
(defun edge1-frame (f)
  (cadr f))
(defun edge2-frame (f)
  (car (cdr (cdr f))))

;;
;; Unit test using a frame system like the one illustrated in the text.
;;
;; First we need the supporting vector operations from Exercise 2.46:
;;
(defun make-vect (x y)
  (cons x y))
(defun xcor-vect (p)
  (car p))
(defun ycor-vect (p)
    (cdr p))

(setq origin (make-vect -2 1))
(setq edge1 (make-vect -1 2))
(setq edge2 (make-vect 4 3))

;;
;; Run the unit test:
;;
(setq f (make-frame origin edge1 edge2))
f
;; ==> ((-2 . 1) (-1 . 2) (4 . 3))

(origin-frame f)
;; ==> (-2 . 1)
(edge1-frame f)
;; ==> (-1 . 2)
(edge2-frame f)
;; ==> (4 . 3)

;;
;; Second constructor and supporting selectors:
;;
(defun make-frame (origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(defun origin-frame (f)
  (car f))
(defun edge1-frame (f)
  (cadr f))
(defun edge2-frame (f)
  (cdr (cdr f)))

;;
;; Again, run the unit tests:
;;
(setq f (make-frame origin edge1 edge2))
f
;; ==> ((-2 . 1) (-1 . 2) 4 . 3)

(origin-frame f)
;; ==> (-2 . 1)
(edge1-frame f)
;; ==> (-1 . 2)
(edge2-frame f)
;; ==> (4 . 3)

;;
;; Note: we will use the same data model that is used in "SICP Picture Language" for Dr. Racket
;;
;; These are the best definitions/constructors/selectors to use for that purpose:
;;
(defun make-frame (origin edge1 edge2)
  (list 'frame origin edge1 edge2))

(defun origin-frame (f)
  (car (cdr f)))
(defun edge1-frame (f)
  (car (cdr (cdr f))))
(defun edge2-frame (f)
  (car (cdr (cdr (cdr f)))))
