(defpackage #:when-packages-test
  (:use #:cl)
  (:import-from #:when-packages
                #:when-packages
                #:when-packages-dynamic))

(in-package #:when-packages-test)

;;; This reader understands (when-packages PACKAGES &body BODY) forms,
;;; whose BODY may contain symbols that reference packages that do not
;;; exist. The body of a WHEN-PACKAGES form will be evaluated if
;;; PACKAGES exist when the form is read. The body of a
;;; WHEN-PACKAGES-DYNAMIC form will be evaluated if PACKAGES exist at
;;; runtime.

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
        "(when-packages (:missing)
            (missing:a missing:b missing:c))"))

; => NIL

(print (read-from-string
        "(when-packages (:when-packages-test.foo)
            (when-packages-test.foo::a
             when-packages-test.foo::b
             when-packages-test.foo::c))"))

; => (PROGN
;      (WHEN-PACKAGES-TEST.FOO::A
;       WHEN-PACKAGES-TEST.FOO::B
;       WHEN-PACKAGES-TEST.FOO::C))

;; The dynamic variant uses eval.
(print (read-from-string
        "(when-packages-dynamic (:when-packages-test.foo)
            (when-packages-test.foo::a
             when-packages-test.foo::b
             when-packages-test.foo::c))"))

; => (WHEN (AND (FIND-PACKAGE ':WHEN-PACKAGES-TEST.FOO))
;      (EVAL
;        (READ-FROM-STRING
;          "(PROGN
;             (WHEN-PACKAGES-TEST.FOO::A
;              WHEN-PACKAGES-TEST.FOO::B
;              WHEN-PACKAGES-TEST.FOO::C))")))
