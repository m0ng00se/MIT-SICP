;;
;; Define the full complex number package:
;;
(load-file "table.el")

;; [WORKING STILL]

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

(defun rectangular? (z)
  (eq (type-tag z) 'rectangular))
(defun polar? (z)
  (eq (type-tag z) 'polar))

;;
;; The way lexical bindings work in emacs, we can't really
;; make this work with the same level of separation/additivity
;; that would normally be desired.
;;

;; Define the rectangular package:
(defun install-rectangular-package ()
  ;; Internal procedures
  (defun real-part-1 (z) (car z))
  (defun imag-part-1 (z) (cdr z))
  (defun magnitude-1 (z)
    (sqrt (+ (square (real-part z))
	     (square (imag-part z)))))
  (defun angle-1 (z)
    (atan (imag-part z) (real-part z)))

  ;; Constructors
  (defun make-from-real-imag (x y) (cons x y))
  (defun make-from-mag-ang (r a)
    (cons (* r (cos a)) (* r (sin a))))

  ;; Tagged constructors
  (defun tag (x) (attach-tag 'rectangular x))
  (defun make-from-real-imag-tagged (x y) 'a)
  (defun make-from-mag-ang-tagged (x y) 'b)
  
  ;; Interface to the rest of the system
  (funcall put 'real-part '(rectangular) #'real-part-1)
  (funcall put 'imag-part '(rectangular) #'imag-part-1)
  (funcall put 'magnitude '(rectangular) #'magnitude-1)
  (funcall put 'angle '(rectangular) #'angle-1)
  (funcall put 'make-from-real-imag 'rectangular #'make-from-real-imag-tagged)
  (funcall put 'make-from-mag-ang 'rectangular #'make-from-mag-ang-tagged)
  'done)

;; Define the polar package:
(defun install-polar-package ()
  ;; Internal procedures
  (defun magnitude-2 (z) (car z))
  (defun angle-2 (z) (cdr z))
  (defun real-part-2 (z)
    (* (magnitude z) (cos (angle z))))
  (defun imag-part-2 (z)
    (* (magnitude z) (sin (angle z))))

  ;; Constructors
  (defun make-from-real-imag (x y)
    (cons (sqrt (+ (square x) (square y)))
	  (atan y x)))
  (defun make-from-mag-ang (r a) (cons r a))

  ;; Tagged constructors
  (defun tag (x) (attach-tag 'polar x))

  ;; Interface to the rest of the system
  (funcall put 'real-part '(polar) #'real-part-2)
  (funcall put 'imag-part '(polar) #'imag-part-2)
  (funcall put 'magnitude '(polar) #'magnitude-2)
  (funcall put 'angle '(polar) #'angle-2)
  (funcall put 'make-from-real-imag 'polar
	   (lambda (x y) (funcall tag (make-from-real-imag x y))))
  (funcall put 'make-from-mag-ang 'polar
	   (lambda (r a) (funcall tag (make-from-mag-ang r a))))
  'done)

;; Generic procedure application:
(defun apply-generic (op &rest args)
  (lexical-let* ((type-tags (mapcar #'type-tag args)))
		(lexical-let* ((proc (get op type-tags)))
			      (if proc
				  (apply proc (mapcar contents args))
				(error
				 "No method for these types -- APPLY-GENERIC" (list op type-tags))))))

(defun real-part (z) (apply-generic 'real-part z))
(defun imag-part (z) (apply-generic 'imag-part z))
(defun magnitude (z) (apply-generic 'magnitude z))
(defun angle (z) (apply-generic 'angle z))

(defun make-from-real-imag (x y)
  (funcall (funcall get 'make-from-real-imag 'rectangular) x y))
(defun make-from-mag-ang (r a)
  (funcall (funcall get 'make-from-mag-ang 'polar) r a))

;;
;; [wORKING]
;;

;; Install complex number packages
(install-rectangular-package)
(install-polar-package)
