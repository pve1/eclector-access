(defpackage #:extended-package-prefix-syntax-test
  (:use #:cl)
  (:export #:reader))

(in-package #:extended-package-prefix-syntax-test)

(eclector-access:enable
 (make-instance 'extended-package-prefix-syntax:reader))

;; Would fail.
(let ((input "CL::(DEFUN ID (X) X))"))
  (princ (nth-value 1 (ignore-errors (read-from-string input)))))

; => Lock on package COMMON-LISP violated when interning ID while in package
;    EXTENDED-PACKAGE-PREFIX-SYNTAX-TEST.
;    See also:
;      The SBCL Manual, Node "Package Locks"
;      The ANSI Standard, Section 11.1.2.1.2

(print 'eclector-access::(a b c))

; => (ECLECTOR-ACCESS::A ECLECTOR-ACCESS::B ECLECTOR-ACCESS::C)
