;; 
;; +-----------------+
;; | RED-BLACK TREES |
;; +-----------------+
;; 
;; A red-black tree is a binary search tree (BST) with an extra bit of
;; information per node, its color, which can be either red or black.
;; As nodes are added to the tree, the color bit is used to dynamically
;; balance the tree so that it retains a height of roughly lg(n).
;;
;; The red-black tree was invented in 1972 by Rudolf Bayer and named the
;; "symmetric binary B-tree" but acquired its modern name in a paper in 
;; 1978 by Leonidas Guibas and Robert Sedgewick entitled "A Dichromatic 
;; Framework for Balanced Trees". The color "red" was chosen because it 
;; was the best-looking color produced by the color laser printer 
;; available to the authors at Xerox PARC.
;;

;;
;; A red-black tree is a binary search tree that meets the following 
;; conditions:
;;
;;  (1) A node is either red or black;
;;  (2) The root is black;
;;  (3) All leaves are black (i.e., all leaves are the same color 
;;      as the root);
;;  (4) Every red node must have two black child nodes, and therefore
;;      it must have a black parent. In other words, there cannot be 
;;      two or more consecutive red nodes.
;;  (5) Every path from a given node to any of its descendant NIL nodes
;;      contains the same number of black nodes.
;;
;; Taken together, these constraint enforce a critical property of 
;; red-black trees: the path from the root to the farthest leaf is 
;; no more than twice as long as the path from the root to the nearest
;; leaf. The result is that the tree is roughly height-balanced.
;;


;;
;; Define the standard tree predicates the procedures:
;;
(define (make-tree node left right)
  (list node left right))
(define the-empty-tree '())
(define empty-tree? null?)
(define tree? list?)

;;
;; We will modify our definition of a node to include both values and colors.
;; The default color will be "red" and the setting for color must be mutable:
;;
(define (make-node value)
  (list value 'red))
(define (node-value node)
  (car node))
(define (node-color node)
  (cadr node))
(define (set-node-color! node color)
  (set-car! (cdr node) color))

;;
;; We need to be able to rotate the tree right and left. We will import 
;; the rotation procedures that are defined below for AVL trees:
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

(define (tree-insert value tree)
  '())


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
  (make-tree value the-empty-tree the-empty-tree))

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
;; "Typedef" convenience procedure:
;;
(define tree-height node-height)

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

(define node-2 (make-tree 2 node-1 node-3))
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
(define node-x (make-tree 'x node-t1 node-t2))
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
	 (- (node-height (node-left tree)) (node-height (node-right tree))))))

(define node-a (make-node 'a))
(define node-b (make-tree 'b the-empty-tree node-a))
(define node-c (make-tree 'c the-empty-tree node-b))
(define node-d (make-tree 'd the-empty-tree node-c))

(define tree-d node-d)
;; ==> (d () (c () (b () (a () ()))))
(tree-height tree-d)
;; ==> 4
(tree-balance tree-d)
;; ==> -3 

(define tree-c (tree-rotate-left tree-d))
;; ==> (c (d () ()) (b () (a () ())))
(tree-height tree-c)
;; ==> 3
(tree-balance tree-c)
;; ==> -1

(define tree-b (tree-rotate-left tree-c))
;; ==> (b (c (d () ())) (a () ()))
(tree-height tree-b)
;; ==> 3 
(tree-balance tree-b)
;; ==> 1

(define tree-a (tree-rotate-left tree-b))
;; ==> (a (b (c (d () ()) ()) ()) ())
(tree-height tree-a)
;; ==> 4
(tree-balance tree-a)
;; ==> 3

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
;; for AVL trees as follows:
;;
(define (tree-insert value tree)
  ;; AVL-adjustment procedure
  (define (avl-adjust-node node)
    (let ((balance (tree-balance node))
	  (key (node-value node))
	  (key-left (node-value (node-left node)))
	  (key-right (node-value (node-right node))))
      (cond 
       ;; (a) Right Rotation
       ((and (> balance  1) (< key key-left)) 
	(tree-rotate-right node))
       ;; (b) Left-Right Rotation
       ((and (> balance  1) (> key key-left)) 
	(let ((new-node (make-tree key
				   (tree-rotate-left (node-left node))
				   (node-right node))))
	  (tree-rotate-right new-node)))
       ;; (c) Left Rotation
       ((and (< balance -1) (> key key-right)) 
	(tree-rotate-left node))
       ;; (d) Right-Left Rotation
       ((and (< balance -1) (< key key-right)) 
	(let ((new-node (make-tree key
				   (node-left node)
				   (tree-rotate-right (node-right node)))))
	  (tree-rotate-left new-node)))
       (else
	node))))
  
  ;; BST-insert with post-insert AVL adjustment
  (cond ((empty-tree? tree)
	 (make-node value))
	(else
	 (let ((current-value (node-value tree)))
	   (cond ((= value current-value) tree)
		 ((< value current-value)
		  (let ((node (make-tree current-value
					 (tree-insert value (node-left tree))
					 (node-right tree))))
		    (avl-adjust-node node)))
		 ((> value current-value)
		  (let ((node (make-tree current-value
					 (node-left tree)
					 (tree-insert value (node-right tree)))))
		    (avl-adjust-node node))))))))

;;
;; Unit Tests:
;;
(define t1 (tree-insert 1 the-empty-tree))
;; ==> (1 () ())
(tree-height t1)
;; ==> 1
(tree-balance t1)
;; ==> 0

(define t2 (tree-insert 2 t1))
;; ==> (1 () (2 () ()))
(tree-height t2)
;; ==> 2
(tree-balance t2)
;; ==> -1

(define t3 (tree-insert 3 t2))
;; ==> (2 (1 () ()) (3 () ()))
(tree-height t3)
;; ==> 2
(tree-balance t3)
;; ==> 0

(define t4 (tree-insert 4 t3))
;; ==> (2 (1 () ()) (3 () (4 () ())))
(tree-height t4)
;; ==> 3
(tree-balance t4)
;; ==> -1

(define t5 (tree-insert 5 t4))
;; ==> (2 (1 () ()) (4 (3 () ()) (5 () ())))
(tree-height t5)
;; ==> 3
(tree-balance t5)
;; ==> -1

(define t6 (tree-insert 6 t5))
;; ==> (3 (2 (1 () ()) ()) (4 () (5 () (6 () ()))))
(tree-height t6)
;; ==> 4
(tree-balance t6)
;; ==> -1

(define t7 (tree-insert 7 t6))
;; ==> (3 (2 (1 () ()) ()) (5 (4 () ()) (6 () (7 () ()))))
(tree-height t7)
;; ==> 4
(tree-balance t7)
;; ==> -1

(define t8 (tree-insert 8 t7))
;; ==> (3 (2 (1 () ()) ()) (5 (4 () ()) (7 (6 () ()) (8 () ()))))
(tree-height t8)
;; ==> 4
(tree-balance t8)
;; ==> -1

(define t9 (tree-insert 9 t8))
;; ==> (5 (3 (2 (1 () ()) ()) (4 () ())) (6 () (7 () (8 () (9 () ())))))
(tree-height t9)
;; ==> 5
(tree-balance t9)
;; ==> -1

(define t10 (tree-insert 10 t9))
;; ==> (5 (3 (2 (1 () ()) ()) (4 () ())) (7 (6 () ()) (8 () (9 () (10 () ())))))
(tree-height t10)
;; ==> 5
(tree-balance t10)
;; ==> -1

(define avl-tree t10)

;;
;; Finally, define a lookup procedure that can be used to retrieve information
;; from the tree:
;;
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

(tree-lookup 1 avl-tree)
;; ==> #t
(tree-lookup 2 avl-tree)
;; ==> #t
(tree-lookup 3 avl-tree)
;; ==> #t
(tree-lookup 4 avl-tree)
;; ==> #t
(tree-lookup 5 avl-tree)
;; ==> #t
(tree-lookup 6 avl-tree)
;; ==> #t
(tree-lookup 7 avl-tree)
;; ==> #t
(tree-lookup 8 avl-tree)
;; ==> #t
(tree-lookup 9 avl-tree)
;; ==> #t
(tree-lookup 10 avl-tree)
;; ==> #t

(tree-lookup 0 avl-tree)
;; ==> #f
(tree-lookup -1 avl-tree)
;; ==> #f
(tree-lookup 11 avl-tree)
;; ==> #f

;;
;; We do not consider deletion operations for either Red-Black or AVL trees in this analysis.
;;

;;
;; This information is derived (loosely) from the implementation found here:
;;
;; http://planet.racket-lang.org/package-source/oesterholt/datastructs.plt/1/0/html/datastructs-avl.html#top
;; http://www.geeksforgeeks.org/avl-tree-set-1-insertion/
;;