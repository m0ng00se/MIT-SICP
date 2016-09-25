;;
;; Alyssa's representation of complex numbers.
;; She has chosen to use the polar representation.
;;
(defun square (x) (* x x))

(defun make-from-real-imag (x y) (cons (sqrt (+ (square x) (square y))) (atan y x)))
(defun make-from-mag-ang (r a) (cons r a))
(defun magnitude (z) (car z))
(defun angle (z) (cdr z))
(defun real-part (z) (* (magnitude z) (cos (angle z))))
(defun imag-real (z) (* (magnitude z) (sin (angle z))))

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
;; Alyssa now needs to update her methods:
;;
(defun make-from-real-imag-polar (x y)
  (attach-tag 'polar
	      (cons (sqrt (+ (square x) (square y)))
		    (atan y x))))
(defun make-from-mag-ang-polar (r a)
  (attach-tag 'polar (cons r a)))
(defun magnitude-polar (z) (car z))
(defun angle-polar (z) (cdr z))
(defun real-part-polar (z)
  (* (magnitude-polar z) (cos (angle-polar z))))
(defun imag-part-polar (z)
  (* (magnitude-polar z) (sin (angle-polar z))))

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
