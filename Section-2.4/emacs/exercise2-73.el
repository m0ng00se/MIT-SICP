
;;
;; Load the necessary libraries:
;;
(load-file "differentiation.el")
(load-file "table.el")

;;
;; Let's also install the procedures we need to support symbolic differentiation:
;;
(defun install-symbolic-differentiator ()
  (put 'deriv '+ (lambda (exp var)
		   (make-sum (deriv (addend exp) var)
			     (deriv (augend exp) var))))
  (put 'deriv '- (lambda (exp var)
		   (make-difference (deriv (minuend exp) var)
				    (deriv (subtrehend exp) var))))
  (put 'deriv '* (lambda (exp var)
		   (make-sum
		    (make-product (multiplier exp)
				  (deriv (multiplicand exp) var))
		    (make-product (deriv (multiplier exp) var)
				  (multiplicand exp)))))
  (put 'deriv '** (lambda (exp var)
		    (make-product (exponent exp)
				  (make-exponentiation
				   (base exp)
				   (make-difference (exponent exp) 1)))))
  'done)

;;
;; Let's also install the procedures we need to support symbolic differentiation:
;;
(install-symbolic-differentiator)

