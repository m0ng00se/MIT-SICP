;;
;; Exercise 2.67
;;
;; Define an encoding tree and a sample message:
;;
;; (define sample-tree
;;  (make-code-tree (make-leaf 'A 4)
;;                  (make-code-tree
;;                   (make-leaf 'B 2)
;;                    (make-code-tree (make-leaf 'D 1)
;;                                    (make-leaf 'C 1)))))
;;
;; (define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
;;
;; Use the "decode" procedure to decode the message and give the result.
;;

;;
;; First let's import the relevant Huffman tree procedures.
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
;; Now, let's define the symbol tree and the sample message, as specified
;; in the problem statement:
;;
(setq sample-tree
      (make-code-tree (make-leaf 'A 4)
		      (make-code-tree
		       (make-leaf 'B 2)
		       (make-code-tree (make-leaf 'D 1)
				       (make-leaf 'C 1)))))

(setq sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)
;; ==> (A D A B B C A)
