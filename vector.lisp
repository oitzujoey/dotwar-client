
(in-package #:dotwar-client)

(defun vector-operation (function args)
  (apply #'map 'vector function args))
(defun v+ (&rest args)
  (vector-operation #'+ args))
(defun v- (&rest args)
  (vector-operation #'- args))
(defun v* (&rest args)
  (vector-operation #'* args))
(defun v/ (&rest args)
  (vector-operation #'/ args))
(defun dot-product (&rest args)
  (apply #'+ (coerce (apply #'v* args)
                     'list)))
(defun magnitude (v)
  (sqrt (dot-product v v)))
(defun normalize (v)
  (v/ v (scalar-to-vector3 (magnitude v))))
(defun scalar-to-vector3 (s)
  (vector s s s))
(defun proj (v n)
  (v* n
      (v/ (scalar-to-vector3 (dot-product v n))
          (scalar-to-vector3 (dot-product n n)))))
(defun perp (v n)
  (v- v (proj v n)))


(defun hamilton-product (a b)
  (vector (+ (* (elt a 0) (elt b 0)) (- (* (elt a 1) (elt b 1))) (- (* (elt a 2) (elt b 2))) (- (* (elt a 3) (elt b 3))))
          (+ (* (elt a 0) (elt b 1)) (* (elt a 1) (elt b 0)) (* (elt a 2) (elt b 3)) (- (* (elt a 3) (elt b 2))))
          (+ (* (elt a 0) (elt b 2)) (- (* (elt a 1) (elt b 3))) (* (elt a 2) (elt b 0)) (* (elt a 3) (elt b 1)))
          (+ (* (elt a 0) (elt b 3)) (* (elt a 1) (elt b 2)) (- (* (elt a 2) (elt b 1))) (* (elt a 3) (elt b 0)))))

(defun quaternion-conjugate (a)
  (vector (elt a 0)
          (- (elt a 1))
          (- (elt a 2))
          (- (elt a 3))))

(defun quaternion-normalize (a)
  (let* ((w (elt a 0))
         (x (elt a 1))
         (y (elt a 2))
         (z (elt a 3))
         (magnitude (sqrt (+ (* w w) (* x x) (* y y) (* z z)))))
    (vector (/ w magnitude)
            (/ x magnitude)
            (/ y magnitude)
            (/ z magnitude))))

(defun vector3-to-quaternion (v)
  (vector 0
          (elt v 0)
          (elt v 1)
          (elt v 2)))

(defun quaternion-to-vector3 (q)
  (vector (elt q 1)
          (elt q 2)
          (elt q 3)))

(defun rotate-vector (v q)
  (quaternion-to-vector3 (hamilton-product q (hamilton-product (vector3-to-quaternion v) (quaternion-conjugate q)))))
