Recitation 9 - Fall 2007 ([PDF](http://people.csail.mit.edu/jastr/6001/fall07/r09.pdf))
=======================================================================================

Binary Trees
------------ 

A binary search tree is a recursively defined data structure which allows for fast searches: lookups take O(log n) time.

In order to support such searches, an invariant on each tree node holds: Each (nonempty) node has a value, and at most two child trees, with the requirement that any value reachable down the left subtree is smaller than the root value, and any value reachable down the right subtree is larger.

```
;; a tree is either an empty tree, or a tree-node (defined below)
(define the-empty-tree null)
(define empty-tree? null?)
(define tree? list?)

(define (make-tree-node value left-subtree right-subtree)
  (list value left-subtree right-subtree))

;; selectors
(define (node-value node)
  (car node))

(define (node-left node)
  (cadr node))

(define (node-right node)
  (caddr node))
```

Problems
-------- 

(1) Complete the definition for ```tree-lookup```, which returns ```true``` if the value is present in the tree.

(2) Fill in the definition for ```tree-insert```, which takes in a tree and a val and returns a new tree with the value added.

**working**
touch
(reading --> red/black trees, or AVL trees)

(working)