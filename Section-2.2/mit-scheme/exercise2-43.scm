;;
;; Exercise 2.43
;;
;; [WORKING]
;;

;;
;; Supporting procedures:
;;
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	    (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

;;
;; Good to have "map" available also, in case we need to refer to it:
;;
(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
	    (map proc (cdr items)))))

;;
;; Let's start by looking at the application of "flatmap" from Exercise 2.42:
;;
(flatmap
 (lambda (rest-of-queens)
   (map (lambda (new-row)
	  (adjoin-position new-row k rest-of-queens))
	(enumerate-interval 1 board-size)))
 (queen-cols (- k 1)))

;;
;; ==================
;; ORIGINAL PROCEDURE
;; ================== 
;;
;; In this application, flatmap only invokes the "queen-cols" procedure once, 
;; to generate the data set it will apply its argument procedure to. Each 
;; invocation of "queen-cols", however, takes the "filter" branch, and thus results
;; in a recursive call back to "flatmap" unless we are invoking "queen-cols" with k=0, 
;; in which case the recursion terminates with the empty board.
;;
;; This means that if we invoke the "queens" procedure with "board-size", the 
;; "queen-cols" procedure will be invoked a total of "board-size+1" times, although
;; only "board-size" of those invocation will result in a call to "flatmap". 
;;              
;; The number of times that the argument procedure to "flatmap" is invoked at each
;; iteration will depend on the number of elements in (queen-cols (- k 1)) at that 
;; iteration point. Whatever this number is, call it n(k) (i.e., it depends on the 
;; iteration k), we will have n(k) invocations of "enumerate-interval" and n(k) * board-size
;; invocations of "adjoin-position". Both of these procedures execute extremely 
;; rapidly: "enumerate-interval" simply counts from 1 up to board-size (let's use 
;; a board-size of 8), and adjoin-position is "syntactic sugar" for "cons", which 
;; also is an extremely fast operation.
;;
;; So to sum up, for 1 invocation of "flatmap", we have the following number of 
;; invocations of the each sub-procedure:
;;
;;  QUEEN-COLS: 1 
;;  ENUMERATE-INTERVAL: n(k)
;;  ADJOIN-POSITION: n(k) * 8
;;
;; where n(k) is the value of (queen-cols (- k 1)).
;;

;;
;; Now let's look at Louis Reasoner's application of flatmap:
;;
(flatmap
 (lambda (new-row)
   (map (lambda (rest-of-queens)
	  (adjoin-position new-row k rest-of-queens))
	(queen-cols (- k 1))))
 (enumerate-interval 1 board-size))

;;
;; Again, expanding the definition of flatmap out can make it easier to see what's going on:
;;
(accumulate append '() (map
			(lambda (new-row)
			  (map (lambda (rest-of-queens)
				 (adjoin-position new-row k rest-of-queens))
			       (queen-cols (- k 1))))
			(enumerate-interval 1 board-size)))

;;
;; Using this method, the following procedures are called the following number of times:
;;
;;  (enumerate-interval 1 board-size) ==> 1 CALL
;;  (lambda (new-row) (map (lambda (rest-of-queens) (adjoin-position new-row k rest-of-queens)) (queen-cols (- k 1)))) ==> BOARD-SIZE CALLS (i.e., 8 CALLS)
;;  (queen-cols (- k  1)) ==> BOARD-SIZE CALLS (i.e., 8 CALLS)
;;  (lambda (rest-of-queens) (adjoin-position new-row k rest-of-queens)) ==> BOARD_SIZE * # OF ELEMENTS IN (QUEEN-COLS (- K 1))
;; 



;;
;; Using this procedure, one invocation of "flatmap" will execute
;; the following procedures the following number of times:
;;
;;  (enumerate-interval 1 board-size) ==> 1 call
;;  (queen-cols (- k 1) ==> 1 call




;;
;; Let's step through some call graphs.
;;
;; As we showed in the previous exercise, if the board size is 4
;; then (queen-cols 3) will be:
;;
;;  ((1 4 2) (2 4 1) (3 1 4) (4 1 3))
;;
;; So the call to flatmap, at the iteration with k=4, might look something like:
;;
(flatmap
 (lambda (rest-of-queens)
   (map (lambda (new-row)
	  (adjoin-position new-row 4 rest-of-queens))
	(enumerate-interval 1 board-size)))
 '((1 4 2) (2 4 1) (3 1 4) (4 1 3)))

(flatmap
 (lambda (rest-of-queens)
   (map (lambda (new-row)
	  (adjoin-position new-row 4 rest-of-queens))
	'(1 2 3 4)))
 '((1 4 2) (2 4 1) (3 1 4) (4 1 3)))

'((1 1 4 2) (2 1 4 2) (3 1 4 2) (4 1 4 2)
 (1 2 4 1) (2 2 4 1) (3 2 4 1) (4 2 4 1)
 (1 3 1 4) (2 3 1 4) (3 3 1 4) (4 3 1 4)
 (1 4 1 3) (2 4 1 3) (3 4 1 3) (4 4 1 3))

;;
;; Let's look at what the call graph looks like using Louis Reasoner's approach:
;;
(flatmap
 (lambda (new-row)
   (map (lambda (rest-of-queens)
	  (adjoin-position new-row k rest-of-queens))
	(queen-cols (- k 1))))
 (enumerate-interval 1 board-size))

(flatmap
 (lambda (new-row)
   (map (lambda (rest-of-queens)
	  (adjoin-position new-row 4 rest-of-queens))
	'((1 4 2) (2 4 1) (3 1 4) (4 1 3))))
 '(1 2 3 4))