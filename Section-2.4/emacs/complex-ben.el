;; 
;; From the text, the complex number representation
;; chosen by Ben Bitdiddle (i.e., rectangular coordinates):
;;
(defun square (x) (* x x))

(defun make-from-real-imag (x y) (cons x y))
(defun make-from-mag-ang (r a) (cons (* r (cos a)) (* r (sin a))))
(defun real-part (z) (car z))
(defun imag-part (z) (cdr z))
(defun magnitude (z) (sqrt (+ (square (real-part z)) (square (imag-part z)))))
(defun angle (z) (atan (imag-part z) (real-part z)))

;;
;; Tagging the data
;;
(defun attach-tag (type-tag contents)
  (cons type-tag contents))
(defun type-tag (datum)
  (if (listp datum)
      (car datum)
    (error "Bad tagged datum -- TYPE-TAG" datum)))
(defun contents (datum)
  (if (listp datum)
      (cdr datum)
    (error "Bad tagged datum -- CONTENTS" datum)))

(defun rectangular (z)
  (eq (type-tag z) 'rectangular))
(defun polar (z)
  (eq (type-tag z) 'polar))

;;
;; Ben now just needs to update his constructors:
;;
(defun make-from-real-imag-rectangular (x y)
  (attach-tag 'rectangular (cons x y)))
(defun make-from-mag-ang-rectangular (r a)
  (attach-tag 'rectangular (cons (* r (cos a)) (* r (sin a)))))
(defun real-part-rectangular (z)
  (car z))
(defun imag-part-rectangular (z)
  (cdr z))
(defun magnitude-rectangular (z)
  (sqrt (+ (square (real-part-rectangular z))
	   (square (imag-part-rectangular z)))))
(defun angle-rectangular (z)
  (atan (imag-part-rectangular z)
	(real-part-rectangular z)))

;;
;; The four selectors above can now be written as:
;;
(defun real-part (z)
  (cond ((rectangular z)
	 (real-part-rectangular (contents z)))
	((polar z)
	 (real-part-polar (contents z)))
	(t
	 (error "Unknown type -- REAL-PART" z))))
(defun imag-part (z)
  (cond ((rectangular z)
	 (imag-part-rectangular (contents z)))
	((polar z)
	 (imag-part-polar (contents z)))
	(t
	 (error "Unknown type -- IMAG-PART" z))))
(defun magnitude (z)
  (cond ((rectangular z)
	 (magnitude-rectangular (contents z)))
	((polar z)
	 (magnitude-polar (contents z)))
	(t
	 (error "Unknown type -- MAGNITUDE" z))))
(defun angle (z)
  (cond ((rectangular z)
	 (angle-rectangular (contents z)))
	((polar z)
	 (angle-polar (contents z)))
	(t
	 (error "Unknown type -- ANGLE" z))))

;;
;; We can likewise redefine the following "general" constructors:
;;
(defun make-from-real-imag (x y)
  (make-from-real-imag-rectangular x y))
(defun make-from-mag-ang (r a)
  (make-from-mag-ang-polar r a))
  
