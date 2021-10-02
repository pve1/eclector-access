(defpackage #:when-packages
  (:use #:cl)
  (:export #:reader
           #:when-packages))

(in-package #:when-packages)

(defclass reader (eclector-access:client) ())

(defvar *allow-unknown-symbols-p*)

(defclass unknown-symbol ()
  ((package-indicator :initarg :package-indicator
                      :reader package-indicator)
   (name :initarg :name :reader name)))

;; Should ideally have a separate function for this.
(defmethod print-object ((object unknown-symbol) stream)
  (format stream "~A::~A"
            (package-indicator object)
            (name object))
  object)

(defmethod eclector.reader:read-common ((client reader)
                                        input-stream
                                        eof-error-p
                                        eof-value)
  (let ((*allow-unknown-symbols-p*
          (if (and (boundp '*allow-unknown-symbols-p*)
                   *allow-unknown-symbols-p*)
              t
              nil)))
    (call-next-method)))

;; Dummy macro
(defmacro when-packages (packages &body body)
  (declare (ignore packages body)))

(defmethod eclector-access:translate-read-result ((client reader) (read-result cons))
  (if (eq 'when-packages (car read-result))
      (destructuring-bind (when-packages packages &rest body)
          read-result
        (declare (ignore when-packages))
        `(when (and ,@(loop :for p :in packages
                            :collect `(find-package ',p)))
           (eval (read-from-string ,(prin1-to-string `(progn ,@body))))))
      read-result))

(defmethod eclector.reader:interpret-symbol ((client reader)
                                             input-stream
                                             package-indicator
                                             symbol-name
                                             internp)
  (cond ((and (string= symbol-name "WHEN-PACKAGES")
              (eq :current package-indicator))
         (setf *allow-unknown-symbols-p* t)
         (call-next-method))

        ((and *allow-unknown-symbols-p*
              (stringp package-indicator)
              (not (find-package package-indicator)))
         (make-instance 'unknown-symbol
                        :name symbol-name
                        :package-indicator package-indicator))

        (t (call-next-method))))
