
(in-package #:dotwar-client)

(defparameter *server* "http://dotwar.pythonanywhere.com")
(defparameter *game* "TESTGAME")
(defparameter *retries* 2)
(defparameter *retries-delay* 3)
(defparameter *authcode* "d0eb0f5c-ae7b-4cd5-974e-d32a83c4931e")
(defparameter *post-send-sleep* 1)
(defparameter *max-acceleration* 1.6e7)
(defparameter *max-speed* 1079251200)
(defparameter *distance-epsilon* 1.0e6)

;; Um...
;; (setf MAXIMA::%E-VAL (exp 1))

(defun ht (&rest alist)
  (let ((ht (make-hash-table :test 'equal)))
    (loop for (key . value) in alist
          do (setf (gethash key ht) value))
    ht))

(defun try-until (function retries interval)
  (let ((response nil)
        (valid nil))
    (loop named retry for i upto retries
          do (progn
               (setf response (funcall function))
               (when (gethash "ok" response)
                 (setf valid t)
                 (return-from retry response))
               (sleep interval)))
    (if valid
        response
        nil)))

(defun default-try-until (function)
  (try-until function *retries* *retries-delay*))

(defun get-games ()
  (prog1
      (gethash "games"
               (default-try-until
                (lambda ()
                  (com.inuoe.jzon:parse (dex:post (concatenate 'string
                                                               *server*
                                                               "/"
                                                               "games"))))))
    (sleep *post-send-sleep*)))

(defun print-games ()
  (let ((games (get-games)))
    (loop for game across games
          do (format t "~A~%" game))
    games))

(defun send (command &rest parameters)
  (prog1
      (default-try-until (lambda ()
                           (com.inuoe.jzon:parse (dex:post (concatenate 'string
                                                                        *server* "/"
                                                                        "game/"
                                                                        *game* "/"
                                                                        command)
                                                           :content parameters))))
    (sleep *post-send-sleep*)))

(defun receive-status ()
  (gethash "game" (send "status")))

(defun receive-scan ()
  (gethash "entities" (send "scan")))

(defun receive-log ()
  (gethash "events" (send "summary")))

(defun send-order (vessel-name authcode acceleration)
  (not (null (send "add_order"
                   (cons "vessel" vessel-name)
                   (cons "authcode" authcode)
                   (cons "order"
                         (com.inuoe.jzon:stringify (ht (cons "task" "burn")
                                                       (cons "args"
                                                             (ht (cons "a" acceleration)))
                                                       (cons "interval" t)
                                                       (cons "time" 0))))))))

(defun get-entity (scan-result name)
  (loop named entity-looper for entity across scan-result
        do (when (equal (gethash "name" entity) name)
             (return-from entity-looper entity))))


(defmethod accelerate-to-point ((acceleration-ref reference)
                                (time-ref reference)
                                target-position
                                target-velocity
                                target-acceleration
                                current-position
                                current-velocity
                                estimated-time-to-intercept)
  (let ((target-position (v+ target-position
                             (v* target-velocity (scalar-to-vector3 estimated-time-to-intercept))
                             (v* target-acceleration
                                 (scalar-to-vector3 (* 1/2
                                                       estimated-time-to-intercept
                                                       estimated-time-to-intercept)))
                             )))
    ;; Transform from 3D to 2D. Little to no information is lost in this transformation.
    (let ((flatten (make-matrix (normalize current-velocity)
                                (normalize (perp (v- target-position current-position) current-velocity)))))
      (flet ((3d-point-to-2d-point (point)
               (matrix-multiply flatten (v- point current-position)))
             (3d-velocity-to-2d-velocity (velocity)
               (matrix-multiply flatten velocity)))
        (let ((current-position-2d (3d-point-to-2d-point current-position))
              (target-position-2d (3d-point-to-2d-point target-position))
              (current-velocity-2d (3d-velocity-to-2d-velocity current-velocity)))
          (flet ((vector2-to-complex (v) (complex (elt v 0) (elt v 1))))
            (let* ((expression `(* ,#C(0 -1)
                                   (LOG (+ (* ,(/ 2 *max-acceleration*)
                                              ,(vector2-to-complex TARGET-POSITION-2D) (EXPT TIME -2))
                                           (* ,(/ -2 *max-acceleration*)
                                              ,(vector2-to-complex CURRENT-VELOCITY-2D) (EXPT TIME -1))))))
                   ;; Thank you Maxima!
                   (diff-expression `(* #C(0 -1)
                                        (+ (* -4
                                              ,(EXPT *MAX-ACCELERATION* -1)
                                              ,(vector2-to-complex TARGET-POSITION-2D)
                                              (EXPT TIME -3))
                                           (* 2
                                              ,(vector2-to-complex CURRENT-VELOCITY-2D)
                                              ,(EXPT *MAX-ACCELERATION* -1)
                                              (EXPT TIME -2)))
                                        (EXPT (+ (* 2
                                                    ,(EXPT *MAX-ACCELERATION* -1)
                                                    ,(vector2-to-complex TARGET-POSITION-2D)
                                                    (EXPT TIME -2))
                                                 (* -2
                                                    ,(vector2-to-complex CURRENT-VELOCITY-2D)
                                                    ,(EXPT *MAX-ACCELERATION* -1)
                                                    (EXPT TIME -1)))
                                              -1))))
              (labels ((let-eval (binds form)
                         (apply (eval `(lambda ,(mapcar #'first binds) ,form)) (mapcar #'second binds)))
                       (f (time)
                         (realpart (let-eval `((time ,time)) expression)))
                       (i-f (time)
                         (imagpart (let-eval `((time ,time)) expression)))
                       (i-df (time)
                         (imagpart (let-eval `((time ,time)) diff-expression)))
                       (newton (time) (- time (/ (i-f time) (i-df time))))
                       (do-n-times (f v n)
                         (loop for i to n
                               do (setf v (funcall f v)))
                         v))
                (let* ((time (do-n-times #'newton (/ 1.0 60.0 60.0) 10))
                       (theta (f time))
                       (theta2d (vector (cos theta) (sin theta))))
                  ;; Transform from 2D to 3D.
                  (let ((inflate (transpose (make-matrix (normalize current-velocity)
                                                         (normalize (perp (v- target-position current-position)
                                                                          current-velocity))))))
                    (flet ((2d-acceleration-to-3d-acceleration (a)
                             (matrix-multiply inflate a)))
                      (reference-set time-ref time)
                      (reference-set acceleration-ref (v* (scalar-to-vector3 *max-acceleration*)
                                                          (2d-acceleration-to-3d-acceleration theta2d))))))))))))))


(defun calculate-acceleration (velocity
                               position
                               target-acceleration
                               target-velocity
                               target-position
                               interval-in-seconds)
  (let (acceleration
        (time 0))
    (loop for i to (1- 1000)
          do (accelerate-to-point (make-reference acceleration)
                                  (make-reference time)
                                  target-position
                                  target-velocity
                                  target-acceleration
                                  position
                                  velocity
                                  time))
    (format t "acceleration: ~A~%" acceleration)
    (format t "time: ~A~%" time)
    acceleration))

(defun calculate-time-to-target (target my-ship)
  (let ((my-entity (get-entity (receive-scan) my-ship))
        (target-entity (get-entity (receive-scan) target)))
    (let ((my-position (gethash "r" my-entity))
          (target-position (gethash "r" target-entity))
          (target-velocity (gethash "v" target-entity))
          (target-acceleration (gethash "a" target-entity))
          (my-velocity (gethash "v" my-entity))
          acceleration
          (time 0))
      (loop for i to (1- 1000)
            do (accelerate-to-point (make-reference acceleration)
                                    (make-reference time)
                                    target-position
                                    target-velocity
                                    target-acceleration
                                    my-position
                                    my-velocity
                                    time))
      time)))

(defun calculate-acceleration-to-target (target my-ship)
  (let ((my-entity (get-entity (receive-scan) my-ship))
        (target-entity (get-entity (receive-scan) target)))
    (let ((my-position (gethash "r" my-entity))
          (target-position (gethash "r" target-entity))
          (target-velocity (gethash "v" target-entity))
          (target-acceleration (gethash "a" target-entity))
          (my-velocity (gethash "v" my-entity))
          acceleration
          (time 0))
      (loop for i to (1- 1000)
            do (accelerate-to-point (make-reference acceleration)
                                    (make-reference time)
                                    target-position
                                    target-velocity
                                    target-acceleration
                                    my-position
                                    my-velocity
                                    time))
      acceleration)))

(defun accelerate-to-target (target my-ship interval-in-seconds)
  (let ((my-entity (get-entity (receive-scan) my-ship))
        (target-entity (get-entity (receive-scan) target)))
    (let ((my-position (gethash "r" my-entity))
          (target-position (gethash "r" target-entity))
          (target-velocity (gethash "v" target-entity))
          (target-acceleration (gethash "a" target-entity))
          (my-velocity (gethash "v" my-entity)))
      (let* ((my-target-acceleration (calculate-acceleration my-velocity
                                                             my-position
                                                             target-acceleration
                                                             target-velocity
                                                             target-position
                                                             interval-in-seconds)))
        (let ((order `(send-order ,my-ship *authcode* ,my-target-acceleration)))
          (format t "~s~%" order)
          (eval order))))))

(defun track-target (target my-ship interval)
  (loop
    (accelerate-to-target target my-ship interval)
    (sleep interval)))

(defun decelerate (ship)
  (let* ((entity (get-entity (receive-scan) ship))
         (velocity (gethash "v" entity))
         (new-acceleration (v- velocity)))
    (let ((order `(send-order ,ship *authcode* ,new-acceleration)))
      (format t "~s~%" order)
      (eval order))))
