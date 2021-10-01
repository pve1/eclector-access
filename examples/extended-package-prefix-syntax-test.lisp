(defpackage #:extended-package-prefix-syntax-test
  (:use #:cl))

(in-package #:extended-package-prefix-syntax-test)

;;; This file demonstrates the extended package prefix syntax as
;;; implemented by SBCL.

(eclector-access:enable
 (make-instance 'extended-package-prefix-syntax:reader))

;; Would fail.
(let ((input "CL::(DEFUN ID (X) X))")
      (eclector.reader:*client*
        (make-instance 'extended-package-prefix-syntax:reader)))
  (princ (nth-value 1 (ignore-errors (eclector.reader:read-from-string input)))))

; => Lock on package COMMON-LISP violated when interning ID while in package
;    EXTENDED-PACKAGE-PREFIX-SYNTAX-TEST.
;    See also:
;      The SBCL Manual, Node "Package Locks"
;      The ANSI Standard, Section 11.1.2.1.2

(print 'eclector-access::(a b c))

; => (ECLECTOR-ACCESS::A ECLECTOR-ACCESS::B ECLECTOR-ACCESS::C)
