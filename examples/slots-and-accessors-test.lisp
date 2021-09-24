(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "slots-and-accessors.lisp" *load-truename*)))

(defpackage #:slots-and-accessors-test
  (:use #:cl))

(in-package #:slots-and-accessors-test)

(eclector-access:enable
 (make-instance 'slots-and-accessors:reader
                :accessor-separator #\/
                :slot-separator #\.))

(defclass foo ()
  ((a :initarg :a)
   (b :initarg :b)
   (c :accessor c)))

(defparameter *foo* (make-instance 'foo :a 1 :b 2))

(print '(defun compute (foo)
         (setf foo/c (+ foo.a foo.b))))

; => (DEFUN COMPUTE (FOO) (SETF (C FOO) (+ (SLOT-VALUE FOO 'A) (SLOT-VALUE FOO 'B))))

(defun compute (foo)
  (setf foo/c (+ foo.a foo.b)))

(compute *foo*)

(print *foo*/c)

; => 3
