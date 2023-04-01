
(in-package #:dotwar-client)

(defclass reference ()
  ((reference :initarg :reference)))

(defmacro make-reference (variable)
  (let ((message (gensym "MESSAGE"))
        (value (gensym "VALUE")))
    `(make-instance 'reference
                    :reference (lambda (,message ,value)
                                 (ccase ,message
                                   (:get ,variable)
                                   (:set (setf ,variable ,value)))))))

(defgeneric reference-set (reference value))
(defgeneric reference-get (reference))

(defmethod reference-set ((reference reference) value)
  (funcall (slot-value reference 'reference) :set value))
(defmethod reference-get ((reference reference))
  (funcall (slot-value reference 'reference) :get nil))
(defun (setf reference-get) (value reference)
  (reference-set reference value))

;; (defmacro with-references (references &rest body)
;;   `(let ((acceleration (reference-get acceleration-ref))
;;          (time (reference-get time-ref)))
;;      ,@body
;;      (reference-set acceleration-ref acceleration)
;;      (reference-set time-ref time)))
