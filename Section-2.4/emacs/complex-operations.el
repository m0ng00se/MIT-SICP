;;
;; General operations that can be used and defined
;; on complex numbers, independent of the data
;; representation used to define them:
;;
(defun add-complex (z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
		       (+ (imag-part z1) (imag-part z2))))

(defun sub-complex (z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
		       (- (imag-part z1) (imag-part z2))))

(defun mul-complex (z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
		     (+ (angle z1) (angle z2))))

(defun div-complex (z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
		     (- (angle z1) (angle z2))))
