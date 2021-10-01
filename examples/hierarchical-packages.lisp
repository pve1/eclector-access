(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "SPLIT-SEQUENCE"))

(defpackage #:hierarchical-packages
  (:use #:cl)
  (:export #:reader))

(in-package #:hierarchical-packages)

;;; This example defines a reader that recognizes hierarchical
;;; packages like those offered by Allegro CL. This implementation is
;;; simple, a proper implementation would need more error checking and
;;; also a hierarchical package -enabled FIND-PACKAGE function.

;;; (https://franz.com/support/documentation/current/doc/packages.htm#resolving-relative-2)

(defclass reader (symbol-patterns:reader) ())

(defun compute-full-package-name (relative-name current-package-name)
  (let* ((prefix-dots (loop :for k :from 0
                            :for c :across relative-name
                            :while (eql c #\.)
                            :finally (return k)))
         (relative-name-without-prefix
           (subseq relative-name prefix-dots))
         (current-package-components
           (split-sequence:split-sequence #\. current-package-name)))
    (format nil "~{~A~^.~}~:[~;.~]~A"
            (subseq current-package-components
                    0
                    (- (length current-package-components)
                       (1- prefix-dots)))
            (not (equal relative-name-without-prefix ""))
            relative-name-without-prefix)))

(defmethod eclector.reader:interpret-symbol ((client reader)
                                             input-stream
                                             package-indicator
                                             symbol-name
                                             internp)
  (if (and (stringp package-indicator)
           (alexandria:starts-with #\. package-indicator))
      (let ((full-name (compute-full-package-name
                        package-indicator
                        (package-name *package*))))
        (when (alexandria:starts-with #\. full-name)
          (error "Package names that start with \".\" will break me."))
        (call-next-method client
                          internp
                          full-name
                          symbol-name
                          internp))
      (call-next-method)))
