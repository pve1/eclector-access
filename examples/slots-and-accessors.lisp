(defpackage #:slots-and-accessors
  (:use #:cl)
  (:export #:reader))

(in-package #:slots-and-accessors)

;;; In this example reader, we define symbol patterns to imitate
;;; WITH-SLOTS and WITH-ACCESSORS.  By default, FOO.ID means
;;; (SLOT-VALUE FOO 'ID), and FOO/ID means (ID FOO).

;;; It is really only suitable to use these patterns in cases where
;;; all symbols involved are present in the current package
;;; (*package*).

(defclass reader (symbol-patterns:reader)
  ((slot-separator :initarg :slot-separator
                   :accessor slot-separator
                   :initform #\.)
   (accessor-separator :initarg :accessor-separator
                       :accessor accessor-separator
                       :initform #\/)))

(defmethod symbol-patterns:translate-symbol ((reader reader)
                                             symbol-name
                                             package-indicator)
  ;; Accessors
  (cond ((find (accessor-separator reader) symbol-name) ; FOO/ID
         (let ((components (split-sequence:split-sequence
                            (accessor-separator reader)
                            symbol-name)))
           ;; Crash on FOO/BAR/ID, FOO.BAR.ID etc.
           (unless (= (length components) 2)
             (error "Confused by symbol ~S." symbol-name))
           `(,(intern (second components) *package*)
             ,(intern (first components) *package*))))
        ;; Slots
        ((find (slot-separator reader) symbol-name) ; FOO.ID
         (let ((components (split-sequence:split-sequence
                            (slot-separator reader)
                            symbol-name)))
           (unless (= (length components) 2)
             (error "Confused by symbol ~S." symbol-name))
           `(slot-value ,(intern (first components) *package*)
                        ',(intern (second components) *package*))))
        (t nil)))
