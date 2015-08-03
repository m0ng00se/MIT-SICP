;;
;; Working definitions
;;
(define the-empty-tree '())
(define empty-tree? null?)
(define tree? list?)

(define (make-tree-node-color value color left-subtree right-subtree)
  (list value color left-subtree right-subtree))
(define (make-tree-node-red value left-subtree right-subtree)
  (make-tree-node-color value 'red left-subtree right-subtree))
(define (make-tree-node-black value left-subtree right-subtree)
  (make-tree-node-color value 'black left-subtree right-subtree))

(define (node-value node)
  (car node))
(define (node-color node)
  (cadr node))
(define (node-left node)
  (caddr node))
(define (node-right node)
  (cadddr node))

(define (tree-lookup value tree)
  (cond ((not (tree? tree)) #f)
	((empty-tree? tree) #f)
	(else
	 (let ((current-value (node-value tree))
	       (left (node-left tree))
	       (right (node-right tree)))
	   (cond ((= current-value value) #t)
		 ((and (> current-value value) (not (empty-tree? left)))
		  (tree-lookup value left))
		 ((and (< current-value value) (not (empty-tree? right)))
		  (tree-lookup value right))
		 (else #f))))))

;;
;; Exercise 5
;; 
;; [WORKING]
;;

;;
;; One simple model for implementing self-balancing trees is a red-black tree.
;;

;; [working] -> implement

;;
;; +-----------+ 
;; | AVL TREES |
;; +-----------+
;;
;; Another relatively simple model for implementing self-balancing trees is 
;; an AVL tree, named after its Soviet inventors Georgy Adelson-Velsky and 
;; E. M. Landis. AVL trees are generally better balanced than red-black trees, 
;; which means that insertion and deletion is generally slower, but actual 
;; lookup of information is faster.
;;
;; To implement an AVL tree, we start with a standard BST insertion. Once 
;; the insertion is complete, we check to see whether both subtrees at the 
;; insertion point are balanced, respectively. If not, we "pivot" or rotate
;; the subtrees to achieve a balanced tree structure, such that the overall 
;; height of final tree is (roughly) on the order of O(lg n).
;;
;; Suppose that T1, T2 and T3 are subtrees of the tree rooted at y (on 
;; the left side) or x (on the right side):
;;
;;          y                             x
;;         / \      Right Rotation       / \ 
;;        x  T3   - - - - - - - - >     T1  y 
;;       / \      < - - - - - - - -        / \
;;      T1 T2       Left Rotation         T2 T3
;;

(define the-empty-tree '())
(define empty-tree? null?)
(define tree? list?)

(define (make-tree value left right)
  (list value left right))
(define (make-node value)
  (make-tree value '() '()))

;;
;; Guard against nulls in these selectors so we can implement recursion 
;; more easily:
;;
(define (node-value node)
  (if (empty-tree? node) '()
      (car node)))
(define (node-left node)
  (if (empty-tree? node) '()
      (cadr node)))
(define (node-right node)
  (if (empty-tree? node) '()
      (caddr node)))

;;
;; Calculate the height of a node recursively:
;;
(define (node-height node)
  (if (empty-tree? node) 0
      (let ((left (node-left node))
	    (right (node-right node)))
	(if (and (empty-tree? left) (empty-tree? right)) 1
	    (+ (max (node-height left) (node-height right)) 1)))))

;;
;; Some unit tests:
;;
(node-height the-empty-tree)
;; ==> 0

(define node-1 (make-node 1))
;; ==> (1 () ())
(define node-3 (make-node 3))
;; ==> (3 () ())
(node-height node-1)
;; ==> 1
(node-height node-3)
;; ==> 1

(define node-2 (make-tree 2 node-1 node-3)
;; ==> (2 (1 () ()) (3 () ()))
(node-height node-2)
;; ==> 2

(define node-6 (make-node 6))
;; ==> (6 () ())
(define node-5 (make-tree 5 node-2 node-6))
;; ==> (5 (2 (1 () ()) (3 () ())) (6 () ()))
(node-height node-6) 
;; ==> 1
(node-height node-5)
;; ==> 3

;;
;; With these procedures in place, we can implement the left- and 
;; right-rotation procedures. We will follow the labeling that was
;; used in the diagrams above for the sake of clarity:
;;
(define (tree-rotate-right node-y)
  (let ((node-x (node-left node-y))
	(node-t3 (node-right node-y)))
    ;;
    ;; Return the tree itself if the left branch is null:
    ;;
    (if (empty-tree? node-x) node-y
	(let ((node-t1 (node-left node-x))
	      (node-t2 (node-right node-x)))
	  (let ((node-y-prime (make-tree (node-value node-y) node-t2 node-t3)))
	    (let ((node-x-prime (make-tree (node-value node-x) node-t1 node-y-prime)))
	      node-x-prime))))))

(define (tree-rotate-left node-x)
  (let ((node-t1 (node-left node-x))
	(node-y (node-right node-x)))    
    ;;
    ;; Return the tree itself if the right branch is null:
    ;;
    (if (empty-tree? node-y) node-x
	(let ((node-t2 (node-left node-y))
	      (node-t3 (node-right node-y)))
	  (let ((node-x-prime (make-tree (node-value node-x) node-t1 node-t2)))
	    (let ((node-y-prime (make-tree (node-value node-y) node-x-prime node-t3)))
	      node-y-prime))))))
	    
;;
;; Unit tests:
;;
(define node-t1 (make-node 't1))
;; ==> (t1 () ())
(define node-t2 (make-node 't2))
;; ==> (t2 () ())
(define node-t3 (make-node 't3))
;; ==> (t3 () ())
(define node-x (make-tree 'x node-t1 node-2))
;; ==> (x (t1 () ()) (t2 () ()))
(define node-y (make-tree 'y node-x node-t3))
;; ==> (y (x (t1 () ()) (t2 () ())) (t3 () ()))

(define tree-1 node-y)
;; ==> (y (x (t1 () ()) (t2 () ())) (t3 () ()))
(node-height tree-1)
;; ==> 3

(define tree-2 (tree-rotate-right tree-1))
;; ==> (x (t1 () ()) (y (t2 () ()) (t3 () ())))
(tree-rotate-left tree-2)
;; ==> (y (x (t1 () ()) (t2 () ())) (t3 () ()))
tree-1
;; ==> (y (x (t1 () ()) (t2 () ())) (t3 () ()))

;;
;; So rotating the tree to the right and left returns the same tree.
;;

(tree-rotate-right tree-1)
;; ==> (x (t1 () ()) (y (t2 () ()) (t3 () ())))
(tree-rotate-right (tree-rotate-right tree-1))
;; ==> (t1 () (x () (y (t2 () ()) (t3 () ()))))
(tree-rotate-right (tree-rotate-right (tree-rotate-right tree-1)))
;; ==> (t1 () (x () (y (t2 () ()) (t3 () ()))))

(tree-rotate-left tree-2)
;; ==> (y (x (t1 () ()) (t2 () ())) (t3 () ()))
(tree-rotate-left (tree-rotate-left tree-2))
;; ==> (t3 (y (x (t1 () ()) (t2 () ())) ()) ())
(tree-rotate-left (tree-rotate-left (tree-rotate-left tree-2)))
;; ==> (t3 (y (x (t1 () ()) (t3 () ())) ()) ())

;; 
;; This is the "workhorse" method of the AVL data structure.
;; It returns the difference of the heights of the left and 
;; right sub-trees of "tree". 
;;
;;  * If the value returned is 0, the two sub-trees are of equal height;
;;  * If the value returned is positive, the left sub-tree is higher than the right;
;;  * If the value returned is negative, the right sub-tree is higher than the left;
;;
;; If the absolute value of the value returned is greater than 1, 
;; we will re-pivot the sub-trees so as to re-establish an overall 
;; tree height on the order of ln(N) while still maintaining the BST 
;; ordering property.
;;
(define (tree-balance tree)
  (cond ((empty-tree? tree) 0)
	(else
	 (- (tree-balance (node-left tree)) (tree-balance (node-right tree))))))

;;
;; Armed with this procedures, we can now proceed to implement the AVL algorithm.
;; 
;; To insert a new node "w" into the tree, perform the following steps:
;;  
;;  (1) Perform standard BST insert for the node "w";
;;  (2) Starting from "w", travel up and find the first unbalanced node, "z".
;;      Let "y" be the child of "z" that comes on the path from w->z and 
;;      let "x" be the grandchild of "z" that comes on the path w->z.
;;  (3) Re-balance the tree by performing the appropriate rotations on the 
;;      subtree rooted at "z", there are four cases to consider:
;;
;;      a. "y" is the left child of "z" and "x" is the left child of "y" 
;       b. "y" is the left child of "z" and "x" is the right child of "y"
;;      c. "y" is the right child of "z" and "x" is the right child of "y"
;;      d. "y" is the right child of "z" and "x" is the left child of "y"
;;
;; We handle these four cases in the following manner:
;;
;;  (a) Right Rotation
;;
;;           z                                   y
;;          / \                                /   \
;;         y  T4    Right Rotation (z) ->    x       z 
;;        / \                               / \     / \
;;       x  T3                             T1 T2   T3 T4
;;      / \ 
;;    T1  T2 
;;
;;  (b) Left-Right Rotation
;; 
;;         z                              z                                  x
;;        / \                            / \                               /   \
;;       y  T4    Left Rotation (y) ->  x   T4   Right Rotation (z) ->   y       z 
;;      / \                            / \                              / \     / \
;;    T1   x                          y   T3                           T1 T2   T3 T4
;;        / \                        / \  
;;       T2 T3                     T1  T2
;;   
;;  (c) Left Rotation
;;
;;     z                                    y
;;    / \                                 /   \ 
;;   T1  y      Left Rotation (z) ->    z       x
;;      / \                            / \     / \ 
;;     x  T4                          T1 T2   T3 T4
;;    / \ 
;;   T2 T3 
;;
;;  (d) Right-Left Rotation
;; 
;;     z                               z                              x 
;;    / \                             / \                           /   \
;;   T1  y   Right Rotation (y) ->   T1  x     Left Rotation ->   z       y
;;      / \                             / \                      / \     / \ 
;;     x  T4                          T2   y                    T1 T2   T3 T4
;;    / \                                 / \
;;   T2 T3                               T3 T4 
;;    

;;
;; With this machinery in place, we can define the tree-insert procedure
;; for AVL trees.
;;

;;
;; We implement the insertion procedure as follows:
;;
(define (tree-insert value tree)
  (define (bst-insert b-value b-tree)
    (cond ((empty-tree? b-tree)
	   (make-node b-value))
	  (else
	   (let ((current (node-value b-tree)))
	     (cond ((= b-value current) b-tree)
		   ((< b-value current)
		    (make-tree current
			       (bst-insert value (node-left b-tree))
			       (node-right b-tree)))
		   ((> b-value current)
		    (make-tree current
			       (node-left b-tree)
			       (bst-insert value (node-right b-tree)))))))))
  (let ((bst-tree (bst-insert value tree)))
    bst-tree))

;;
;; This information is derived (loosely) from the implementation found here:
;;
;; http://planet.racket-lang.org/package-source/oesterholt/datastructs.plt/1/0/html/datastructs-avl.html#top
;; http://www.geeksforgeeks.org/avl-tree-set-1-insertion/
;;