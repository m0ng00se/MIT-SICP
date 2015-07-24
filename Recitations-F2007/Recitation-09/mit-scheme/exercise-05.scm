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

;;
;; [EXPLAIN MORE ABOUT AVL ROTATION]
;;

(define the-empty-tree '())
(define empty-tree? null?)
(define tree? list?)

(define (make-tree value left right)
  (list value left right))
(define (make-node value)
  (make-tree value '() '()))

(define (node-value node)
  (car node))
(define (node-left node)
  (cadr node))
(define (node-right node)
  (caddr node))

;;
;; Calculate the height of a node recursively:
;;
(define (node-height node)
  (if (null? node) 0
      (let ((left (node-left node))
	    (right (node-right node)))
	(if (and (null? left) (null? right)) 1
	    (+ (max (node-height left) (node-height right)) 1)))))

;;
;; Implementing an AVL tree will be easier if we allow ourselves the 
;; luxury of updating the left and right branches in-place:
;;
(define (node-set-left! node left)
  (set-car! (cdr node) left))
(define (node-set-right! node right)
  (set-car! (cdr (cdr node)) right))

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
;; right-rotation procedures:
;;


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

;; (working) -> explain
(define (tree-insert value tree)

  ;; Copy src node to dst node
  (define (node-copy src dst)
    (node-set-value! dst (node-value src))
    (node-set-left! dst (node-left src))
    (node-set-right! dst (node-right src))
    (node-set-height! dst (node-height src)))

  ;; BST insertion, modifying tree in-place
  (define (tree-insert-bst)
    (cond ((empty-tree? tree)
	   (let ((new-node (tree-new-node value)))
	     (node-copy new-node tree)))
	  (else
	   '())))

  ;; Run tree insection procedure
  (tree-insert-bst))


  (define (tree-insert-proc)
    (cond ((empty-tree? tree)
	   (let ((new-node (tree-new-node value)))
	     (node-set-value! tree (node-value new-node))
	     (node-set-left! tree (node-left new-node))
	     (node-set-right! tree (node-right new-node))
	     (node-set-height! tree (node-height new-node))))
	  ((< value (
    '())
  (let ((new-tree (tree-insert-proc)))
    new-tree))
  
  (define (tree-insert-proc)
    (cond ((empty-tree? tree) (tree-new-node value))
	  ((< value (node-value tree)) (tree-insert value (node-left tree)))
	  ((> value (node-value tree)) (tree-insert value (node-right tree)))
	  (else
	   tree)))
  (let ((new-tree (tree-insert-proc)))
    new-tree))


;;
;; These trees have the properties that they "re-balance" their own nodes 
;; dynamically as nodes are added to or removed from the tree. More information
;; on AVL tres can be found on Google and Wikipedia.
;;
;; We will redefine "tree-insert" to function as an AVL tree.
;;
;; We give an implementation of AVL trees as follows:
;;

;;
;; AVL trees work by keeping the tree "as balanced" as possible at each node.
;; We define a "balance factor" at each node, which is (-1) x (weight of left branch) + 
;; (+1) x (weight of right branch). A balance factor of -1, 0, or 1 is considered
;; "balanced". Otherwise, we need to readjust the height of the tree.
;;
;; The first thing we need is a way to compute the height of the tree at a given node:
;;
(define (compute-height tree)
  (let ((h 0))
    (if (not (empty-tree (node-left tree)))
	(if (> 
  

;;
;; This information is derived (loosely) from the implementation found here:
;;
;; http://planet.racket-lang.org/package-source/oesterholt/datastructs.plt/1/0/html/datastructs-avl.html#top
;;