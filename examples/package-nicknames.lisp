(defpackage #:package-nicknames
  (:use #:cl)
  (:export #:reader))

(in-package #:package-nicknames)

(defclass reader (eclector-access:client)
  ((nickname-table :initarg :nickname-table
                   :accessor nickname-table
                   :initform (make-hash-table :test 'equal))))

(defmethod initialize-instance :after ((reader reader) &key nicknames)
  (loop :for (nick package-name) :on nicknames :by #'cddr
        :while (and nick package-name)
        :do (setf (gethash (string nick) (nickname-table reader))
                  (string package-name))))

(defun lookup-package (reader nickname)
  (gethash (string nickname) (nickname-table reader)))

(defmethod eclector.reader:interpret-symbol ((client reader)
                                             input-stream
                                             package-indicator
                                             symbol-name
                                             internp
                                             &aux pkg)
  (if (and (stringp package-indicator)
           (setf pkg (lookup-package client package-indicator)))
      (call-next-method client
                        input-stream
                        pkg
                        symbol-name
                        internp)
      (call-next-method)))
