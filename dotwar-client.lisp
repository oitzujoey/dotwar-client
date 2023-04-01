
(in-package #:dotwar-client)


(defparameter *server* "http://dotwar.pythonanywhere.com")
(defparameter *game* "TESTGAME")
(defparameter *retries* 2)
(defparameter *retries-delay* 3)
(defparameter *post-send-sleep* 1)
(defparameter *window-size* 800)
(defparameter *window-width* *window-size*)
(defparameter *window-height* *window-size*)
(defparameter *window-usage-fraction* 0.99)
(defparameter *my-ship* "My Quintessence")

(defvar *orientation* #(1 0 0 0))
(defparameter *scale* 1.0)


(defun quadratic-bezier (x1 y1 bx by x2 y2)
  (let ((cs (max 2 (sketch::pen-curve-steps (sketch::env-pen sketch::*env*)))))
    (apply #'polyline (mapcan (lambda (v)
                                (sketch::quadratic-bezier-point v
                                                                (list x1 y1)
                                                                (list bx by)
                                                                (list x2 y2)))
                              (alexandria:iota (1+ cs)
                                               :step (/ 1 cs))))))


(defclass entity ()
  ((position :initarg :position)
   (velocity :initarg :velocity)
   (acceleration :initarg :acceleration)))

(defmethod print-object ((entity entity) stream)
  (format stream
          "(make-entity ~a ~a ~a)"
          (coerce (slot-value entity 'position) 'list)
          (coerce (slot-value entity 'velocity) 'list)
          (coerce (slot-value entity 'acceleration) 'list)))

(defmethod make-entity ((position vector) (velocity vector) (acceleration vector))
  (make-instance 'entity
                 :position position
                 :velocity velocity
                 :acceleration acceleration))

(defgeneric project-3d-to-2d (point max-displacement))
(defmethod project-3d-to-2d ((point vector) max-displacement)
  (let ((point (rotate-vector point *orientation*))
        (max-displacement (/ max-displacement *scale*)))
    (let ((half-window-width (/ *window-width* 2.0))
          (half-window-height (/ *window-height* 2.0))
          (scaling-factor (/ 1.0 max-displacement)))
      (let ((x (* (+ (* (elt point 0) scaling-factor half-window-width) half-window-width) *window-usage-fraction*))
            (y (* (+ (* (elt point 1) scaling-factor half-window-height) half-window-height) *window-usage-fraction*))
            (z (* (elt point 2) scaling-factor (/ (+ half-window-width half-window-height) 2.0))))
        (vector x
                y)))))

(defmethod project-3d-to-size ((point vector) max-displacement)
  (let ((point (rotate-vector point *orientation*))
        (max-displacement (/ max-displacement *scale*)))
    (let* ((half-window-width (/ *window-width* 2.0))
           (half-window-height (/ *window-height* 2.0))
           (half-window-distance (/ (+ half-window-width half-window-height) 2.0)))
      (+ (* (+ (elt point 2) max-displacement) (/ 1.0 max-displacement) half-window-distance)
         half-window-distance))))


(defun draw-predicted-path (entity color time max-displacement)
  (with-pen (make-pen :stroke color)
    (let* ((3d-position (slot-value entity 'position))
           (2d-position (project-3d-to-2d 3d-position max-displacement))
           (3d-velocity (slot-value entity 'velocity))
           (2d-velocity (project-3d-to-2d (map 'vector
                                               #'+
                                               3d-position
                                               3d-velocity)
                                          max-displacement))
           (3d-acceleration (slot-value entity 'acceleration))
           (2d-acceleration (project-3d-to-2d (map 'vector
                                               #'+
                                               3d-position
                                               3d-acceleration)
                                              max-displacement))
           (half-time (/ time 2))
           (3d-mid-point (v+ 3d-position
                             (v* 3d-velocity (scalar-to-vector3 half-time))))
           (2d-mid-point (project-3d-to-2d 3d-mid-point max-displacement))
           (3d-end-point (v+ 3d-position
                             (v* 3d-velocity (scalar-to-vector3 time))
                             (v* 3d-acceleration (scalar-to-vector3 (* 1/2 time time)))))
           (2d-end-point (project-3d-to-2d 3d-end-point max-displacement))
           (x1 (elt 2d-position 0))
           (y1 (elt 2d-position 1))
           (b1 (elt 2d-mid-point 0))
           (b2 (elt 2d-mid-point 1))
           (x2 (elt 2d-end-point 0))
           (y2 (elt 2d-end-point 1)))
      (quadratic-bezier x1 y1 b1 b2 x2 y2))))

(defun draw-entity-slot (entity slot scale color max-displacement)
  (with-pen (make-pen :stroke color)
    (let* ((3d-position (slot-value entity 'position))
           (2d-position (project-3d-to-2d 3d-position max-displacement))
           (3d-attribute (slot-value entity slot))
           (2d-attribute (project-3d-to-2d (map 'vector
                                                #'+
                                                3d-position
                                                (map 'vector (lambda (e) (* e scale)) 3d-attribute))
                                           max-displacement))
           (x1 (elt 2d-position 0))
           (y1 (elt 2d-position 1))
           (x2 (elt 2d-attribute 0))
           (y2 (elt 2d-attribute 1)))
      (line x1 y1 x2 y2))))

(defun draw-entity (entity color scale max-displacement)
  (with-pen (make-pen :fill color)
    (let* ((3d-position (slot-value entity 'position))
           (2d-position (project-3d-to-2d 3d-position max-displacement)))
      (circle (elt 2d-position 0)
              (elt 2d-position 1)
              (* (log (project-3d-to-size 3d-position max-displacement))
                 scale)))))


(defun view ()
  (let (all-entities
        all-scan-entities
        event-entities
        defender-scan-entities
        attacker-scan-entities
        my-ship)
    (loop for event across (receive-log)
          do (when (equal (gethash "type" event) "burn")
               (let ((args (gethash "args" event)))
                 (let ((entity (make-entity (gethash "position" args) #(0 0 0) (gethash "a" args))))
                   (push entity event-entities)))))
    (let ((scan (receive-scan)))
      (loop for json-entity across scan
            do (let ((entity (make-entity (gethash "r" json-entity) (gethash "v" json-entity) (gethash "a" json-entity))))
                 (if (equal *my-ship* (gethash "name" json-entity))
                     (setf my-ship entity)
                     (if (= 0 (gethash "team" json-entity))
                         (push entity defender-scan-entities)
                         (push entity attacker-scan-entities)))
                 (push entity all-scan-entities)
                 (push entity all-entities))))
    (let ((max-displacement (apply #'max
                                   (mapcar (lambda (e)
                                             (apply #'max (map 'list (lambda (a) (abs a)) (slot-value e 'position))))
                                           all-entities)))
          (earth (make-entity #(0 0 0) #(0 0 0) #(0 0 0))))

      ;; Plot
      (defsketch system ((title *game*)
                         (width *window-width*)
                         (height *window-height*))
        (background +black+)
        (loop for entity in event-entities
              do (draw-entity entity +blue+ 0.2 max-displacement))
        (draw-entity earth +white+ 0.4 max-displacement)
        (loop for entity in all-scan-entities
              do (draw-entity-slot entity 'velocity 24 +red+ max-displacement))
        (loop for entity in all-scan-entities
              do (draw-entity-slot entity 'acceleration (* 24 24 1/2) +cyan+ max-displacement))
        (loop for entity in all-scan-entities
              do (draw-predicted-path entity +green+ (* 24 2) max-displacement))
        (loop for entity in attacker-scan-entities
              do (draw-entity entity +red+ 0.4 max-displacement))
        (loop for entity in defender-scan-entities
              do (draw-entity entity +yellow+ 0.4 max-displacement))
        (draw-entity my-ship +green+ 0.4 max-displacement))))
  
  (defmethod kit.sdl2:mousemotion-event :after ((window system) time-stamp mask x y xr yr)
    (flet ((left-mouse-button-clicked-p ()
             (= mask 1)))
      (when (left-mouse-button-clicked-p)
        (let ((xr (/ xr *window-width*))
              (yr (/ yr *window-height*)))
          (setf *orientation*
                (quaternion-normalize (hamilton-product *orientation*
                                                        (quaternion-normalize (vector 1 (- yr) 0 (- xr))))))))))

  (make-instance 'system))
