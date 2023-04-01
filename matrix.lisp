
(in-package #:dotwar-client)

(defclass matrix ()
  ((matrix :initarg :matrix)))

(defun make-matrix (&rest vectors)
  (make-instance 'matrix
                 :matrix (map 'vector (lambda (vector) (coerce vector 'vector)) vectors)))

(defun make-n-by-m-matrix (n m)
  (make-instance 'matrix
                 :matrix (apply #'vector
                                (loop for i to (1- n)
                                      collecting (make-array m
                                                             :initial-element 0)))))

(defmethod print-object ((matrix matrix) stream)
  (format stream "(make-matrix ~{~a~^ ~})" (coerce (slot-value matrix 'matrix) 'list)))

(defmethod matrix-multiply ((left matrix) (right vector))
  (let ((new-vector (make-array (length (slot-value left 'matrix)) :initial-element 0)))
    (loop for i from 0 to (1- (length (elt (slot-value left 'matrix) 0)))
          do (loop for j from 0 to (1- (length (slot-value left 'matrix)))
                   do (incf (elt new-vector j)
                            (* (elt (elt (slot-value left 'matrix) j) i)
                               (elt right i)))))
    new-vector))

(defmethod transpose ((matrix matrix))
  ;; (print matrix)
  (let* ((matrix-body (slot-value matrix 'matrix))
         (n (length (elt matrix-body 0)))
         (m (length matrix-body))
         (new-matrix (make-n-by-m-matrix n m)))
    (loop for i to (1- n)
          do (loop for j to (1- m)
                   do (setf (elt (elt (slot-value new-matrix 'matrix) i) j) (elt (elt matrix-body j) i))))
    new-matrix))
