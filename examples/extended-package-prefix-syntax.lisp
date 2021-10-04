(defpackage #:extended-package-prefix-syntax
  (:use #:cl)
  (:export #:reader))

(in-package #:extended-package-prefix-syntax)

;;; This example defines a reader that is able to handle SBCL's
;;; extended package prefix syntax, i.e.:
;;;
;;; 'foo::(bar quux zot) == '(foo::bar foo::quux foo::zot)

(defclass reader (symbol-patterns:reader) ())

(defvar *alternate-package* nil)

(defmethod eclector.reader:interpret-token ((client reader)
                                            input-stream
                                            token
                                            escape-ranges)
  ;; Eclector will signal an error on "foo::", so we handle it here
  ;; and perform a recursive READ to get the next form with
  ;; *ALTERNATE-PACKAGE* bound.
  (handler-bind ((eclector.reader:symbol-name-must-not-end-with-package-marker
                   (lambda (c)
                     (declare (ignore c))
                     (let ((max-escape (loop :for (begin . end) :in escape-ranges
                                             :maximize end)))
                       (if (and (alexandria:ends-with-subseq "::" token)
                                (< max-escape (- (length token) 1)))
                           (let* ((package-name ; All except "::"
                                    (subseq token 0 (- (length token) 2)))
                                  (package (find-package package-name)))
                             (unless package
                               (error "No package named ~A." package-name))
                             (let ((*alternate-package* package))
                               (return-from eclector.reader:interpret-token
                                 (eclector.reader:read input-stream t nil t)))))))))
    (call-next-method)))

(defmethod eclector.reader:interpret-symbol ((client reader)
                                             input-stream
                                             package-indicator
                                             symbol-name
                                             internp)
  (if *alternate-package*
      (call-next-method client
                        input-stream
                        (if (eq package-indicator :current)
                            (package-name *alternate-package*)
                            package-indicator)
                        symbol-name
                        internp)
      (call-next-method)))
