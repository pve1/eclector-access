(defpackage #:slots-and-accessors-test
  (:use #:cl))

(in-package #:slots-and-accessors-test)

;;; Sometimes, code that contains many (SLOT-VALUE ...) forms can feel
;;; a bit verbose. In this example, the reader is used to provide an
;;; alternative to WITH-ACCESSORS and WITH-SLOTS.

(eclector-access:enable
 (make-instance 'slots-and-accessors:reader
                :accessor-separator #\/
                :slot-separator #\.))

(defclass foo ()
  ((a :initarg :a)
   (b :initarg :b)
   (c :accessor c)))

(defparameter *foo* (make-instance 'foo :a 1 :b 2))

(defun compute (foo)
  (setf foo/c (+ foo.a foo.b)))

(compute *foo*)

(print *foo*/c)

; => 3
