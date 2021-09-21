(defpackage #:smalltalk-keywords-test
  (:use #:cl))

(in-package #:smalltalk-keywords-test)

(eclector-access:enable (make-instance 'smalltalk-keywords:reader
                                       :operator 'send))

(print '(foo: bar baz))

; => ((SEND FOO) BAR BAZ)
