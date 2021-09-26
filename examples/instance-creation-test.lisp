(defpackage #:instance-creation-test
  (:use #:cl))

(in-package #:instance-creation-test)

;;; This example shows how to create new instances of a class by
;;; capitalizing its name.

(eclector-access:enable (make-instance 'instance-creation:reader))

(defclass foo ()
  ((a :initarg :a :reader a :initform nil)
   (b :initarg :b :reader b :initform nil)))

(defmethod print-object ((object foo) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "A: ~S B: ~S" (a object) (b object)))
  object)

(print '(Foo)) ; => (MAKE-INSTANCE 'FOO)

(print '(Foo :a 1 :b 2)) ; => (MAKE-INSTANCE 'FOO :A 1 :B 2)

(print '(Foo :a (Foo :a 1)
             :b (Foo :b 2)))

; => (MAKE-INSTANCE 'FOO :A (MAKE-INSTANCE 'FOO :A 1)
;                        :B (MAKE-INSTANCE 'FOO :B 2))

(print (Foo)) ; => #<FOO A: NIL B: NIL>

(print (Foo :a 1 :b 2)) ; => #<FOO A: 1 B: 2>

(print (Foo :a (Foo :a 1)
            :b (Foo :b 2)))

; => #<FOO A: #<FOO A: 1 B: NIL> B: #<FOO A: NIL B: 2>>
