;;
;; Exercise 2.45
;;
;; "right-split" and "up-split" can be expressed as instances of a general splitting operation.
;; Define a procedure "split" with the property that evaluating:
;;
;;  (define right-split (split beside below))
;;  (define up-split (split below beside))
;;
;; produces procedures "right-split" and "up-split" with the same behavior as the ones already defined.
;;

;;
;; We define "split" as follows:
;;
(defun split (op1 op2)
  (defun split-inner (painter n)
    (if (= n 0)
	painter
      (let ((smaller (split-inner painter (- n 1))))
	(funcall op1 painter (funcall op2 smaller smaller)))))
  (split-inner))

;;
;; We can now define the two split procedures as follows:
;;
(setq right-split (split beside below))
(setq up-split (split below beside))
