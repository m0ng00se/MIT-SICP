;;
;; Exercise 2.69
;;
;; The following procedure takes as its argument a list of symbol-frequency pairs (where no
;; symbol appears in more than one pair) and generates a Huffman encoding tree according to
;; the Huffman algorithm.
;;
;;  (define (generate-huffman-tree pairs)
;;   (successive-merge (make-leaf-set pairs)))
;;
;; Make-leaf-set is the procedure given above that transforms the list of pairs into an ordered
;; set of leaves. Successive-merge is the procedure you must write, using make-code-tree to
;; successively merge the smallest-weight elements of the set until there is only one element left,
;; which is the desired Huffman tree. (This procedure is slightly tricky, but not really complicated.
;; If you find yourself designing a complex procedure, then you are almost certainly doing something
;; wrong. You can take significant advantage of the fact that we are using an ordered set representation.)
;;

;;
;; First let's reimport all the same Huffman tree code we used before.
;;
(load-file "exercise2-68.el")

;;
;; In addition, we require two methods defined in the text for building up leaf-sets:
;;
(defun adjoin-set (x set)
  (cond ((null set) (list x))
	((< (weight x) (weight (car set))) (cons x set))
	(t
	 (cons (car set)
	       (adjoin-set x (cdr set))))))

(defun make-leaf-set (pairs)
  (if (null pairs)
      '()
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car pair)         ;; symbol
			     (car (cdr pair)))  ;; frequency
		  (make-leaf-set (cdr pairs))))))

;;
;; First let's play with "make-leaf-set" to get a sense for how it performs:
;;
(make-leaf-set '((a 8) (b 3)))
;; ==> ((leaf b 3) (leaf a 8))
(make-leaf-set '((a 8) (b 3) (c 100) (d 1)))
;; ==> ((leaf d 1) (leaf b 3) (leaf a 8) (leaf c 100))

;;
;; So "adjoin-set" generates a list of leaves, ordered by frequency, and sorted
;; in order of increasing frequency (which, indeed, is evident from looking at the
;; definition of the procedure).
;;

;;
;; We define a recursive "successive-merge" procedure which combines the smallest two elements:
;;
(defun successive-merge (pairs)
  (if (= (length pairs) 1)
      (car pairs)
    (let ((first (car pairs))
	  (second (car (cdr pairs)))
	  (rest (cdr (cdr pairs))))
      (successive-merge (adjoin-set (make-code-tree first second)
				    rest)))))

;;
;; And finally generate the Huffman tree:
;;
(defun generate-huffman-tree (pairs)
  (successive-merge (make-leaf-set pairs)))

;;
;; Let's see if we can get it to work:
;;
(setq tree1 (generate-huffman-tree '((a 4) (b 2) (c 1) (d 1))))
;; ==> ((leaf a 4) ((leaf b 2) ((leaf d 1) (leaf c 1) (d c) 2) (b d c) 4) (a b d c) 8)

(encode-symbol 'a tree1)
;; ==> (0)
(encode-symbol 'b tree1)
;; ==> (1 0)
(encode-symbol 'c tree1)
;; ==> (1 1 1)
(encode-symbol 'd tree1)
;; ==> (1 1 0)

;;
;; Note that the "successive-merge" procedure we've defined here gives the same
;; Huffman tree encoding as that defined "by hand" in the text:
;;
(encode '(a d a b b c a) tree1)
;; ==> (0 1 1 0 0 1 0 1 0 1 1 1 ...)

(decode (encode '(a d a b b c a) tree1) tree1)
;; ==> (a d a b b c a)

;;
;; It's instructive to step through the call graph for a simple example, to
;; better visualize how "successive-merge" works. The argument passed into
;; successive merge is the result of calling the "make-leaf-set" procedure,
;; which sorts the weighted pairs into a kind of makeshift priority queue:
;;
(make-leaf-set '((a 4) (b 2) (c 1) (d 1)))
;; ==> ((leaf d 1) (leaf c 1) (leaf b 2) (leaf a 4))

(successive-merge '((leaf d 1) (leaf c 1) (leaf b 2) (leaf a 4)))
;; first <- (leaf d 1)
;; second <- (leaf c 1)
;; rest <- ((leaf b 2) (leaf a 4))
(successive-merge (adjoin-set (make-code-tree '(leaf d 1) '(leaf c 1)) '((leaf b 2) (leaf a 4))))
(successive-merge (adjoin-set '((leaf d 1) (leaf c 1) (d c) 2) '((leaf b 2) (leaf a 4))))
(successive-merge '((leaf b 2) ((leaf d 1) (leaf c 1) (d c) 2) (leaf a 4)))
;; first <- (leaf b 2)
;; second <- ((leaf d 1) (leaf c 1) (d c) 2)
;; rest <- ((leaf a 4))
(successive-merge (adjoin-set (make-code-tree '(leaf b 2) '((leaf d 1) (leaf c 1) (d c) 2)) '((leaf a 4))))
(successive-merge (adjoin-set '((leaf b 2) ((leaf d 1) (leaf c 1) (d c) 2) (b d c) 4) '((leaf a 4))))
(successive-merge '((leaf a 4) ((leaf b 2) ((leaf d 1) (leaf c 1) (d c) 2) (b d c) 4)))
;; first <- (leaf a 4)
;; second <- ((leaf b 2) ((leaf d 1) (leaf c 1) (d c) 2) (b d c) 4)
;; rest <- ()
(successive-merge (adjoin-set (make-code-tree '(leaf a 4) '((leaf b 2) ((leaf d 1) (leaf c 1) (d c) 2) (b d c) 4)) '()))
(successive-merge (adjoin-set '((leaf a 4) ((leaf b 2) ((leaf d 1) (leaf c 1) (d c) 2) (b d c) 4) (a b d c) 8) '()))
(successive-merge '(((leaf a 4) ((leaf b 2) ((leaf d 1) (leaf c 1) (d c) 2) (b d c) 4) (a b d c) 8)))
;; ==> (((leaf a 4) ((leaf b 2) ((leaf d 1) (leaf c 1) (d c) 2) (b d c) 4) (a b d c) 8))
