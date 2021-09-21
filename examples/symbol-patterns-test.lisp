(defpackage #:symbol-patterns-test
  (:use #:cl))

(in-package #:symbol-patterns-test)

;;; In this example, if a symbol starts with "?", we look it up in the
;;; mood table.

(eclector-access:enable
 (make-instance 'symbol-patterns:reader
                :patterns (lambda (symbol)
                            (when (and (not (zerop (length (symbol-name symbol))))
                                       (eql (aref (symbol-name symbol) 0) #\?))
                              `(gethash ',symbol *mood*)))))

(defvar *mood*)
(defvar *bored* (make-hash-table :test 'eq))
(defvar *excited* (make-hash-table :test 'eq))

(let ((*mood* *bored*))
  (setf ?hello "Hello."
        ?bye "Bye."))

(let ((*mood* *excited*))
  (setf ?hello "Hellooo!"
        ?bye "Bye bye!"))

(defun hello ()
  (print ?hello))

(defun bye ()
  (print ?bye))

(let ((*mood* *bored*))
  (hello)                               ; => "Hello."
  (bye))                                ; => "Bye."

(let ((*mood* *excited*))
  (hello)                               ; => "Hellooo!"
  (bye))                                ; => "Bye bye!"
