;;
;; Exercise 2.68
;;
;; The "encode" procedure takes as arguments a message and a tree and produces the list of bits that
;; gives the encoded message.
;;
;;  (define (encode message tree)
					;    (if (null? message)
;;       '()
;;       (append (encode-symbol (car message) tree)
;;               (encode (cdr message) tree))))
;;
;; "Encode-symbol" is a procedure, which you must write, that returns the list of bits that encodes
;; a given symbol according to a given tree. You should design encode-symbol so that it signals an
;; error if the symbol is not in the tree at all. Test your procedure by encoding the result you
;; obtained in exercise 2.67 with the sample tree and seeing whether it is the same as the original
;; sample message.
;;

;;
;; First let's reimport all the same Huffman tree code we used before.
;;
(load-file "exercise2-67.el")

;;
;; Now define the "encode" procedure.
;;
;; First we define some of the supporting procedures:
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
;; Let's test it out using the same sample tree as before:
;;
(setq sample-tree
      (make-code-tree (make-leaf 'A 4)
		      (make-code-tree
		       (make-leaf 'B 2)
		       (make-code-tree (make-leaf 'D 1)
				       (make-leaf 'C 1)))))

;;
;; Let's run some unit tests:
;;
(left-branch sample-tree)
;; ==> (leaf A 4)

(left-branch (right-branch sample-tree))
;; ==> (leaf B 2)

;;
;; Inspecting the sample tree, we deduce that the following encodings should hold:
;;
;;  A <== 0
;;  B <== 10
;;  C <== 111
;;  D <== 110
;;

;;
;; Let's see if those are the encodings that we get from the procedure:
;;
(encode-symbol 'A sample-tree)
;; ==> (0)
(encode-symbol 'B sample-tree)
;; ==> (1 0)
(encode-symbol 'C sample-tree)
;; ==> (1 1 1)
(encode-symbol 'D sample-tree)
;; ==> (1 1 0)

;;
;; Let's try to encode a symbol not in the tree:
;;
;;(encode-symbol 'e sample-tree)
;; ==> Bad symbol: ENCODE-SYMBOL e

;;
;; Looks good so far..
;;
;; Let's see if we can encode the message that was given in the previous exercise:
;;
(encode '(A D A B B C) sample-tree)
;; ==> (0 1 1 0 0 1 0 1 0 1 1 1)

(decode (encode '(A D A B B C) sample-tree) sample-tree)
;; ==> (A D A B B C)
