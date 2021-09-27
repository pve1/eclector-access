(defpackage #:symbol-patterns
  (:use #:cl)
  (:export #:reader
           #:translate-symbol))

(in-package #:symbol-patterns)

;;; The reader contains a pattern function (taking one symbol as its
;;; argument) that may translate the symbol into something else.  This
;;; example treats a return value of NIL as meaning "no translation
;;; necessary".

;;; Something like this could be useful if a software project has a
;;; commonly occuring pattern that could be handled with a simple
;;; naming convention for variables (well-documented, of course).

(defclass reader (eclector-access:client)
  ())

(defgeneric translate-symbol (reader symbol-name package-indicator)
  (:method (reader symbol-name package-indicator)
    nil))

(defmethod eclector.reader:interpret-symbol ((client reader)
                                             input-stream
                                             package-indicator
                                             symbol-name
                                             internp)
  (let ((match (translate-symbol client
                                 symbol-name
                                 package-indicator)))
    (if match
        match
        (call-next-method))))
