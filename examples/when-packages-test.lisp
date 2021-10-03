(defpackage #:when-packages-test
  (:use #:cl)
  (:import-from #:when-packages
                #:when-packages))

(in-package #:when-packages-test)

;;; This reader understands (when-packages PACKAGES &body BODY) forms,
;;; whose BODY may contain symbols that reference packages that do not
;;; exist. The body is evaluated only if PACKAGES exist at runtime.

(eclector-access:enable (make-instance 'when-packages::reader))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ignore-errors (delete-package :when-packages-test.foo)))

;; Package does not exist
(when-packages (:when-packages-test.foo)
  (print 'when-packages-test.foo::a))

; => (prints nothing)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (make-package :when-packages-test.foo))

;; Package exists
(when-packages (:when-packages-test.foo)
  (print 'when-packages-test.foo::b))

; => WHEN-PACKAGES-TEST.FOO::B

(print (read-from-string
        "(when-packages (:foo)
            (foo:a foo:b foo:c))"))

; => (WHEN (AND (FIND-PACKAGE ':FOO))
;      (EVAL (READ-FROM-STRING "(PROGN (FOO::A FOO::B FOO::C))")))