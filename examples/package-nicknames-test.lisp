(defpackage #:package-nicknames-test
  (:use #:cl))

(in-package #:package-nicknames-test)

;;; Activate reader
(eclector-access:enable
 (make-instance 'package-nicknames:reader
                :nicknames '("E" "ECLECTOR-ACCESS"
                             "P" "PACKAGE-NICKNAMES"
                             "C" "CL")))

(print 'e:client)
(print 'p:reader)
(print (c:list 1 2))

;; Will not work, of course: (find-symbol "LIST" "C")
