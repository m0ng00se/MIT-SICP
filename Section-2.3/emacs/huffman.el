;;
;; Huffman codes are an effective technique for compressing data, savings of 20% to 90% are typical.
;; The Huffman algorithm constructs a variable-length prefix-code based on a table of the frequencies
;; of occurrence of each character in a file, and builds up an optimal way of representing each character
;; as a (variable-length) binary string. Frequently-occurring characters are given short codes, while
;; rarely-occurring characters are given long codes. The optimal code for a given file is always represented
;; as a FULL binary tree, where every non-leaf node has two children.
;;
;; An excellent reference on Huffman codes is given in the CLR text, Chapter 17 on Greedy Algorithms.
;;

;;
;; Procedures for generating the leaves of a Huffman tree:
;;
(defun make-leaf (symbol weight)
  (list 'leaf symbol weight))
(defun leaf? (object)
  (eq (car object) 'leaf))
(defun symbol-leaf (x) (car (cdr x)))
(defun weight-leaf (x) (car (cdr (cdr x))))

;;
;; Procedures for constructing and manipulating the Huffman tree:
;;
(defun make-code-tree (left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))
(defun left-branch (tree) (car tree))
(defun right-branch (tree) (car (cdr tree)))
(defun symbols (tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
    (car (cdr (cdr tree)))))
(defun weight (tree)
  (if (leaf? tree)
      (weight-leaf tree)
    (car (cdr (cdr (cdr tree))))))

;;
;; Procedures for decoding messages using a Huffman tree:
;;
(defun decode (bits tree)
  (defun decode-1 (bits current-branch)
    (if (null bits)
	'()
      (let ((next-branch (choose-branch (car bits) current-branch)))
	(if (leaf? next-branch)
	    (cons (symbol-leaf next-branch)
		  (decode-1 (cdr bits) tree))
	  (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(defun choose-branch (bit branch)
  (cond ((= bit 0) (left-branch branch))
	((= bit 1) (right-branch branch))
	(t
	 (error "Bad bit -- CHOOSE BRANCH" bit))))

;;
;; Procedures for encoding messages using a Huffman tree:
;;
(defun element-of-set? (x set)
  (cond ((null set) '())
	((equal x (car set)) t)
	(t
	 (element-of-set? x (cdr set)))))

(defun encode-symbol (symbol tree)
  ;; Build up the encoding using constant time "cons",
  ;; and then reverse the list once done .. should be
  ;; faster than repeatedly invoking "append".
  (defun encode-1 (tree-list encoded)
    (if (leaf? tree-list)
	(reverse encoded)
      (let ((left (left-branch tree-list))
	    (right (right-branch tree-list)))
	(let ((symbols-left (symbols left))
	      (symbols-right (symbols right)))
	  (cond ((element-of-set? symbol symbols-left)
		 (encode-1 left (cons 0 encoded)))
		((element-of-set? symbol symbols-right)
		 (encode-1 right (cons 1 encoded)))
		(t
		 (error "Bad symbol: ENCODE-SYMBOL" symbol)))))))
  (encode-1 tree '()))

(defun encode (message tree)
  (if (null message)
      '()
    (append (encode-symbol (car message) tree)
	    (encode (cdr message) tree))))

;;
;; Procedures for generating the Huffman tree itself:
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

(defun successive-merge (pairs)
  (if (= (length pairs) 1)
      (car pairs)
    (let ((first (car pairs))
	  (second (car (cdr pairs)))
	  (rest (cdr (cdr pairs))))
      (successive-merge (adjoin-set (make-code-tree first second)
				    rest)))))

(defun generate-huffman-tree (pairs)
  (successive-merge (make-leaf-set pairs)))
