(defpackage #:instance-creation
  (:use #:cl)
  (:export #:reader))

(in-package #:instance-creation)

;;; This example defines a reader that recognizes capitalized symbols
;;; and translates them into MAKE-INSTANCE forms.
;;;
;;; E.g. "(Foo :a 1 :b 2)" becomes (MAKE-INSTANCE 'FOO :A 1 :B 2).
;;;
;;; This reader could make code that uses many different classes and
;;; that relies heavily on MAKE-INSTANCE a bit easier to write, and
;;; possibly to read.

(defclass reader (symbol-patterns:reader)
  ((buffer :accessor buffer :initform nil)
   (inner-readtable :initarg :inner-readtable
                    :accessor inner-readtable
                    :initform (eclector.readtable:copy-readtable
                               eclector.readtable:*readtable*))))

(defmethod initialize-instance :after ((reader reader) &key)
  (setf (eclector.readtable:readtable-case (inner-readtable reader))
        :invert))

(defclass spliced-form ()
  ((form :initarg :form :reader form)))

(defmethod eclector-access:translate-read-result ((reader reader) (result cons))
  (typecase (car result)
    (spliced-form
     (append (form (car result))
             (eclector-access:translate-read-result reader (cdr result))))
    (t (cons (eclector-access:translate-read-result reader (car result))
             (eclector-access:translate-read-result reader (cdr result))))))

(defmethod eclector.reader:read-common ((client reader)
                                        input-stream
                                        eof-error-p
                                        eof-value)
  (let ((eclector.reader:*readtable* (inner-readtable client)))
    (call-next-method)))

(defmethod eclector.reader:interpret-symbol ((client reader)
                                             input-stream
                                             package-indicator
                                             symbol-name
                                             internp)
  (if (or (some #'lower-case-p symbol-name)
          (some #'lower-case-p (string package-indicator)))
      (let* ((package (case package-indicator
                        (:current *package*)
                        (t (find-package (string-upcase package-indicator)))))
             (real-symbol
               (intern (string-upcase symbol-name) package)))
        (make-instance 'spliced-form
                       :form `(make-instance ',real-symbol)))
      (call-next-method)))
