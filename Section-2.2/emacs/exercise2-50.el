
;; Load previous procedures:
(load-file "exercise2-49.el")

;;
;; The "transform-painter" procedure from the text:
;;
(defun transform-painter (painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
	(painter
	 (make-frame new-origin
		     (sub-vect (m corner1) new-origin)
		     (sub-vect (m corner2) new-origin)))))))

;;
;; Answers to the questions:
;;
(defun flip-horiz (painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)   ;; new origin
		     (make-vect 0.0 0.0)   ;; new end of edge 1
		     (make-vect 1.0 1.0))) ;; new end of edge 2

(defun rotate180 (painter)
  (transform-painter painter
		     (make-vect 1.0 1.0)   ;; new origin
		     (make-vect 0.0 1.0)   ;; new end of edge 1
		     (make-vect 1.0 0.0))) ;; new end of edge 2

(defun rotate270 (painter)
  (transform-painter painter
		     (make-vect 0.0 1.0)   ;; new origin
		     (make-vect 0.0 0.0)   ;; new end of edge 1
		     (make-vect 1.0 1.0))) ;; new end of edge 2
