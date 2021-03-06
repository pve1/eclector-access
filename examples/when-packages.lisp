(defpackage #:when-packages
  (:use #:cl)
  (:export #:reader
           #:when-packages
           #:when-packages-dynamic))

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

;;; Dummy macros

(defmacro when-packages-dynamic (packages &body body)
  (declare (ignore packages body)))

(defmacro when-packages (packages &body body)
  (declare (ignore packages body)))

(defun check-no-unknown-symbols (tree)
  (labels ((walk (tree)
             (typecase tree
               (cons
                (walk (car tree))
                (walk (cdr tree)))
               (t (when (typep tree 'unknown-symbol)
                    (error "Unknown symbol ~A." tree))))))
    (walk tree)
    tree))

(defmethod eclector-access:translate-read-result ((client reader) (read-result cons))
  (case (car read-result)
    (when-packages-dynamic
        (destructuring-bind (when-packages-dynamic packages &rest body)
            read-result
          (declare (ignore when-packages-dynamic))
          `(when (and ,@(loop :for p :in packages
                              :collect `(find-package ',p)))
             (eval (read-from-string ,(prin1-to-string `(progn ,@body)))))))
    (when-packages
     (destructuring-bind (when-packages packages &rest body)
         read-result
       (declare (ignore when-packages))
       (when (loop :for p :in packages :always (find-package p))
         (check-no-unknown-symbols read-result)
         `(progn ,@body))))
    (t read-result)))

(defmethod eclector.reader:interpret-symbol ((client reader)
                                             input-stream
                                             package-indicator
                                             symbol-name
                                             internp)
  (cond ((and (or (string= symbol-name "WHEN-PACKAGES")
                  (string= symbol-name "WHEN-PACKAGES-DYNAMIC"))
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
