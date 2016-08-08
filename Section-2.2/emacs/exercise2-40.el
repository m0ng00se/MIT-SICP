
;;
;; First (re-)define the "accumulate" procedure:
;;
(defun accumulate (op initial sequence)
  (defun iter (result rest)
    (if (null rest)
	result
      (iter (funcall op result (car rest))
	    (cdr rest))))
  (iter initial sequence))

;;
;; Define the "enumerate-interval" procedure:
;;
(defun enumerate-interval (i j)
  (defun iter (count total)
    (cond ((<= count j) (iter (+ count 1) (append total (list count))))
	  (t
	   total)))
  (iter i '()))

;;
;; Test the "enumerate-interval" procedure:
;;
(enumerate-interval 1 5)
;; ==> (1 2 3 4 5)
(enumerate-interval 1 10)
;; ==> (1 2 3 4 5 6 7 8 9 10)
(enumerate-interval 1 1)
;; ==> (1)
(enumerate-interval 1 2)
;; ==> (1 2)
(enumerate-interval 1 0)
;; ==> nil

;;
;; "unique-pairs" can be defined by simply abstracting the procedure
;; definition already given in the text, which defines the generation of pairs.
;;
(defun uniuque-pairs (n)
  (accumulate append
	      '()
	      (mapcar (lambda (i)
			(mapcar (lambda (j) (list i j))
				(enumerate-interval 1 (- i 1))))
		      (enumerate-interval i n))))

;;
;; This definition is workable, but as was pointed out in the text, the combination
;; of mapping and accumulating is so common that it's best to isolate it as its
;; own separate procedure:
;;
(defun flatmap (proc seq)
  (accumulate #'append '() (mapcar proc seq)))

;;
;; Using this abstraction, we can now write:
;;
(defun unique-pairs (n)
  (flatmap (lambda (i)
	     (mapcar (lambda (j) (list i j))
		     (enumerate-interval 1 (- i 1))))
	   (enumerate-interval 1 n)))

(unique-pairs 1)
;; ==> nil
(unique-pairs 2)
;; ==> ((2 1))
(unique-pairs 3)
;; ==> ((2 1) (3 1) (3 2))
(unique-pairs 5)
;; ==> ((2 1) (3 1) (3 2) (4 1) (4 2) (4 3) (5 1) (5 2) (5 3) (5 4))

;;
;; Define helper functions:
;;
(defun even? (n)
  (= (mod n 2) 0))
(defun square (n)
  (* n n))
(defun filter (pred seq)
  (cond ((null seq) '())
	((funcall pred (car seq))
	 (cons (car seq)
	       (filter pred (cdr seq))))
	(t
	 (filter pred (cdr seq)))))

;;
;; In order to implement the "prime-sum-pairs" procedure, we need to
;; implement a prime-testing routine, which we will import from Section 1.2:
;;
(defun expmod (base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (mod (square (expmod base (/ exp 2) m)) m))
	(t
	 (mod (* base (expmod base (- exp 1) m)) m))))

(defun prime? (n)
  (defun get-random-a ()
    (+ 2 (random (- n 4))))
  (defun test (a)
    (= (expmod a (- n 1) n) 1))
  (cond ((= n 2) t)
	((= n 3) t)
	((= n 4) nil)
	((= n 5) t)
	(t
	 (and (test (- n 1))
	      (test (- n 2))
	      (test (get-random-a))
	      (test (get-random-a))
	      (test (get-random-a))))))

(prime? 3)
;; ==> t
(prime? 1000999)
;; ==> t

;;
;; We also need the supporting procedures defined in the text:
;;
(defun prime-sum? (pair)
  (prime? (+ (car pair) (cadr pair))))
(defun make-pair-sum (pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

;;
;; With all this machinery in place, we can re-define the "prime-sum-pairs"
;; procedure as follows:
;;
(defun prime-sum-pairs (n)
  (mapcar #'make-pair-sum
	  (filter #'prime-sum? (unique-pairs n))))
;; ==> ((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11))
