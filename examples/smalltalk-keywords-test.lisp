(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "smalltalk-keywords.lisp" *load-truename*)))

(defpackage #:smalltalk-keywords-test
  (:use #:cl))

(in-package #:smalltalk-keywords-test)

(eclector-access:enable
 (make-instance 'smalltalk-keywords:reader
                :translation (lambda (symbol)
                               (list 'send symbol))))

(print '(foo: bar baz))

; => ((SEND FOO) BAR BAZ)
