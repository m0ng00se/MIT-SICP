;;
;; Exercise 2.65
;;
;; Use the results of Exercises 2.63 and 2.64 to give O(n) implementations of "union-set"
;; and "intersection-set" for sets implemented as (balanced) binary trees.
;;

;;
;; First let's import the supporting tree operations that we're going to require:
;;
(load-file "exercise2-64.el")

;;
;; Now we need to include the O(n) set operations we've defined
;; earlier, for both "union" and "intersection". These operate
;; on ordered sets (in linear time):
;;
(defun union-set (set1 set2)
  (cond ((null set1) set2)
	((null set2) set1)
	(t
	 (let ((elem1 (car set1))
	       (elem2 (car set2)))
	   (cond ((= elem1 elem2)
		  (cons elem1 (union-set (cdr set1) (cdr set2))))
		 ((< elem1 elem2)
		  (cons elem1 (union-set (cdr set1) set2)))
		 ((> elem1 elem2)
		  (cons elem2 (union-set set1 (cdr set2)))))))))

(defun intersection-set (set1 set2)
  (if (or (null set1) (null set2))
      '()
    (let ((x1 (car set1))
	  (x2 (car set2)))
      (cond ((= x1 x2)
	     (cons x1 (intersection-set (cdr set1) (cdr set2))))
	    ((< x1 x2)
	     (intersection-set (cdr set1) set2))
	    ((> x1 x2)
	     (intersection-set set1 (cdr set2)))))))

;;
;; Finally, let's define the "union-set" and "intersection-set" procedures:
;;
(defun union-tree (tree1 tree2)
  (list->tree (union-set (tree->list-1 tree1)
			 (tree->list-1 tree2))))

(defun intersection-tree (tree1 tree2)
  (list->tree (intersection-set (tree->list-1 tree1)
				(tree->list-1 tree2))))

;;
;; Let's run some unit tests:
;;
(setq t1 (list->tree '(1)))
;; ==> (1 nil nil)
(setq t2 (list->tree '(1 2)))
;; ==> (1 nil (2 nil nil))
(setq t3 (list->tree '(1 2 3)))
;; ==> (2 (1 nil nil) (3 nil nil))
(setq t4 (list->tree '(2 3 4)))
;; ==> (3 (2 nil nil) (4 nil nil))
(setq t5 (list->tree '(4 5 6)))
;; ==> (5 (4 nil nil) (6 nil nil))

(union-tree '() '())
;; ==> nil
(union-tree t1 '())
;; ==> (1 nil nil)
(union-tree '() t1)
;; ==> (1 nil nil)
(union-tree (list->tree '(1)) (list->tree '(1)))
;; ==> (1 nil nil)
(union-tree (list->tree '(1 2)) (list->tree '(1 2)))
;; ==> (1 nil (2 nil nil))
(union-tree t1 t2)
;; ==> (1 nil (2 nil nil))
(union-tree t2 t1)
;; ==> (1 nil (2 nil nil))
(union-tree t3 t4)
;; ==> (2 (1 nil nil) (3 nil (4 nil nil)))
(union-tree t3 t5)

(intersection-tree '() '())
;; ==> nil
(intersection-tree t1 '())
;; ==> nil
(intersection-tree '() t1)
;; ==> nil
(intersection-tree (list->tree '(1)) (list->tree '(1)))
;; ==> (1 nil nil)
(intersection-tree (list->tree '(1 2)) (list->tree '(1 2)))
;; ==> (1 nil (2 nil nil))
(intersection-tree t1 t2)
;; ==> (1 nil nil)
(intersection-tree t2 t1)
;; ==> (1 nil nil)
(intersection-tree t3 t4)
;; ==> (2 nil (3 nil nil))
(intersection-tree t3 t5)
;; ==> nil

;;
;; Why do these procedures run in O(n) time?
;;
;; Consider the "union-tree" procedure. The "tree->list" procedure runs in O(n) time, and this
;; procedure is called twice. The "union-set" procedure runs in O(n), and the final call to
;; "list->tree" also runs in O(n) time. So we have four procedure calls, each of which run in
;; linear time. Invoking the "union-tree" may, therefore, take (up to) 4 times longer than
;; executing any single one of these O(n) procedures, but the sum of four O(n) is still a procedure
;; which runs in linear O(n) time.
;;
;; The same arguments can be made for "intersection-tree".
;;
