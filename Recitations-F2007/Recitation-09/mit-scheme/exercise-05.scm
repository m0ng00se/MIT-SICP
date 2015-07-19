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

;;
;; To implement an AVL tree, we start with a standard BST insertion. Once 
;; the insertion is complete, we check to see whether both subtrees at the 
;; insertion point are balanced, respectively. If not, we "pivot" or rotate
;; the subtrees to achieve a balanced tree structure, such that the overall 
;; height of final tree is (roughly) on the order of O(lg n).
;;

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
;; Implementing an AVL tree is generally easier if we use tree nodes that 
;; keep a running record of their height in the tree. A tree node will thus 
;; look something like the following:
;;
;;  '(node-value left-subtree right-subtree node-height)
;;
;; This data structure is also much easier to implement if we allow ourselves
;; the convenience of mutating the state of tree node as the insertion procedure
;; runs.
;;
(define the-empty-tree '())
(define empty-tree? null?)
(define tree? list?)

(define (node-value node)
  (car node))
(define (node-left node)
  (cadr node))
(define (node-right node)
  (caddr node))
(define (node-height node)
  (cadddr node))

(define (node-set-left! node left)
  (set-car! (cdr node) left))
(define (node-set-right! node right)
  (set-car! (cdr (cdr node)) right))
(define (node-set-height! node height)
  (set-car! (cdr (cdr (cdr node))) height))

;; (working) -> explain
(define (tree-new-node value)
  (list value '() '() 1))

;; (working) -> explain
(define (tree-height tree)
  (cond ((empty-tree? tree) 0)
	(else
	 (node-height tree))))

;; (working) -> explain
(define (tree-balance tree)
  (cond ((empty-tree? tree) 0)
	(else
	 (- (tree-balance (node-left tree)) (tree-balance (node-right tree))))))

;; (working) -> explain
(define (tree-insert value tree)
  (let ((height (+ (max (tree-height (node-left tree)) (tree-height (node-right tree))) 1)))
    '())
  (cond ((empty-tree? tree) (tree-new-node value))
	((< key (node-value tree)) (tree-insert value (node-left tree)))
	((> key (node-value tree)) (tree-insert value (node-right tree)))
	(else
	 tree)))
  



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