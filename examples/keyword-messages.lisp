(defpackage #:keyword-messages
  (:use #:cl)
  (:export #:reader))

(in-package #:keyword-messages)

;;; This example defines a reader that recognizes "keyword messages"
;;; similar to those found in Smalltalk.  In a message form, the first
;;; element is the recipient of the message and the rest is any number
;;; of alternating keywords and arguments.  The keywords are of the
;;; form "foo:" (which would normally be illegal in Common Lisp).
;;; This is, of course, purely an exercise in syntax.  No other
;;; aspects of Smalltalk message passing are implemented.
;;;
;;; An example message looks like this:
;;;
;;;   (1 to: 5 do: #'print)
;;;
;;; and it is simply translated into:
;;;
;;;   (TO=DO= 1 5 #'PRINT)
;;;
;;; If a message consists solely of keywords, then it is interpreted as a
;;; unary message chain. For example:
;;;
;;;   (1.5 floor: 1+: prin1-to-string:)
;;;
;;; becomes:
;;;
;;;   (PRIN1-TO-STRING (1+ (FLOOR 1.5)))
;;;
;;; A recipient can itself be a message form.  This means that the
;;; following is also valid:
;;;
;;;   ((1 to: 5) third: 1+:)
;;;
;;; this becomes:
;;;
;;;   (1+ (THIRD (TO= 1 5)))
;;;

;;; Keywords

(defclass message-keyword ()
  ((%symbol :initarg :symbol :initform (error "Must supply symbol."))))

(defmethod print-object ((object message-keyword) stream)
  (print-unreadable-object (object stream)
    (princ (name object) stream)
    (princ ":" stream)))

(defun keyword-message-form-p (form)
  (and (consp form)
       (typep (second form) 'message-keyword)))

(defmethod name ((message-keyword message-keyword))
  (symbol-name (slot-value message-keyword '%symbol)))

(defmethod keyword-package ((message-keyword message-keyword))
  (symbol-package (slot-value message-keyword '%symbol)))

;;; Reader

(defclass reader (eclector-access:client)
  ((marker :initarg :marker :reader marker :initform "=")))

;; Translates (foo a: 1 b: 2) to (a=b= foo 1 2).
(defun translate-keyword-message-form (reader message)
  (destructuring-bind (recipient &rest parameters)
      message
    (let ((marker (marker reader))
          keywords
          arguments)
      (if (every (lambda (x) (typep x 'message-keyword)) ; Unary chain
                 parameters)
          (loop :with form = (eclector-access:translate-read-result
                              reader recipient)
                :for kw :in parameters
                :do (setf form `(,(intern (format nil "~A" (name kw))
                                          (keyword-package kw))
                                 ,form))
                :finally (return form))
          (progn
            (unless (evenp (length parameters))
              (error "Missing argument in message: ~S." message))
            (loop :for (kw arg) :on parameters :by #'cddr
                  :collect kw :into %keywords
                  :collect arg :into %arguments
                  :finally (setf keywords %keywords
                                 arguments %arguments))
            `(,(intern (format nil "~:{~A~A~}"
                               (mapcar (lambda (kw) (list (name kw) marker)) keywords))
                       (keyword-package (first keywords)))
              ,(eclector-access:translate-read-result reader recipient)
              ,@(mapcar (lambda (arg)
                          (eclector-access:translate-read-result reader arg))
                        arguments)))))))

;; Walks trees applying the translation to keyword message forms.
(defmethod eclector-access:translate-read-result ((reader reader)
                                                  (read-result cons))
  (if (keyword-message-form-p read-result)
      (translate-keyword-message-form reader read-result)
      (cons (eclector-access:translate-read-result reader (car read-result))
            (eclector-access:translate-read-result reader (cdr read-result)))))

(defmethod eclector.reader:interpret-token ((client reader)
                                            input-stream
                                            token
                                            escape-ranges)
  ;; Should check edge cases.
  (if (eql #\: (aref token (1- (length token))))
      (let ((symbol (call-next-method client
                                      input-stream
                                      (subseq token 0 (1- (length token)))
                                      escape-ranges)))
        (make-instance 'message-keyword :symbol symbol))
      (call-next-method)))
