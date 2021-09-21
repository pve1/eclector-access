(defpackage #:smalltalk-keywords
  (:use #:cl)
  (:export #:reader))

(in-package #:smalltalk-keywords)

(defclass reader (eclector-access:client)
  ((translation :initarg :translation
                :accessor translation
                :initform (lambda (symbol)
                            (intern (symbol-name symbol) :keyword)))))

(defmethod eclector.reader:interpret-token ((client reader)
                                            input-stream
                                            token
                                            escape-ranges)
  ;; Should check for edge cases.
  (if (eql #\: (aref token (1- (length token))))
      (funcall (translation client)
               (call-next-method client
                                 input-stream
                                 (subseq token 0 (1- (length token)))
                                 escape-ranges))
      (call-next-method)))
