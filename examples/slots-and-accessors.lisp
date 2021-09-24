(defpackage #:slots-and-accessors
  (:use #:cl)
  (:export #:reader))

(in-package #:slots-and-accessors)

;;; In this example reader, we define symbol patterns to imitate
;;; WITH-SLOTS and WITH-ACCESSORS. By default, FOO.ID means
;;; (SLOT-VALUE FOO 'ID), and FOO/ID means (ID FOO).

(defclass reader (symbol-patterns:reader)
  ((slot-separator :initarg :slot-separator
                   :accessor slot-separator
                   :initform #\.)
   (accessor-separator :initarg :accessor-separator
                       :accessor accessor-separator
                       :initform #\/)))

(defmethod initialize-instance :after ((reader reader) &key)
  (setf (symbol-patterns:translation reader) #'translate))

(defun translate (symbol)
  (let ((name (symbol-name symbol))
        (reader eclector.reader:*client*))
    ;; Accessors
    (cond ((find (accessor-separator reader) name) ; FOO/ID
           (let ((components (split-sequence:split-sequence
                              (accessor-separator reader)
                              name)))
             (unless (= (length components) 2)
               (error "Confused by symbol ~S." symbol))
             ;; Could use FIND-SYMBOL too.
             `(,(intern (second components) *package*)
               ,(intern (first components) *package*))))
          ;; Slots
          ((find (slot-separator reader) name) ; FOO.ID
           (let ((components (split-sequence:split-sequence
                              (slot-separator reader)
                              name)))
             (unless (= (length components) 2)
               (error "Confused by symbol ~S." symbol))
             `(slot-value ,(intern (first components) *package*)
                          ',(intern (second components) *package*))))
          (t nil))))
