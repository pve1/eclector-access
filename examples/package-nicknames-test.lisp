(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "package-nicknames.lisp" *load-truename*)))

(defpackage #:package-nicknames-test
  (:use #:cl))

(in-package #:package-nicknames-test)

(eclector-access:enable
 (make-instance 'package-nicknames:reader
                :nicknames '("E" "ECLECTOR-ACCESS"
                             "P" "PACKAGE-NICKNAMES"
                             "C" "CL")))

(print 'e:client)
(print 'p:reader)
(print (c:list 1 2))

;; Will not work, of course: (find-symbol "LIST" "C")
