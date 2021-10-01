(defpackage #:hierarchical-packages-test
  (:use #:cl))

(in-package #:hierarchical-packages-test)

;;; This file demonstrates a reader that recognizes hierarchical
;;; packages, similar to the those implemented by Allegro CL. This
;;; only works on the reader level, and does not affect the behaviour
;;; of CL:FIND-PACKAGE etc.
;;;
;;; (https://franz.com/support/documentation/current/doc/packages.htm#resolving-relative-2)

(eclector-access:enable
 (make-instance 'hierarchical-packages:reader))

(defpackage #:hierarchical-packages-test.foo (:use #:cl))
(defpackage #:hierarchical-packages-test.foo.bar (:use #:cl))
(defpackage #:hierarchical-packages-test.foo.bar.baz (:use #:cl))

(in-package #:hierarchical-packages-test)

(print '.foo::a) ; => HIERARCHICAL-PACKAGES-TEST.FOO::A

(print '.foo.bar::a) ; :=> HIERARCHICAL-PACKAGES-TEST.FOO.BAR::A

(print '.foo.bar.baz::a) ; => HIERARCHICAL-PACKAGES-TEST.FOO.BAR.BAZ::A

(in-package #:hierarchical-packages-test.foo.bar)

(print '.::a) ; => A

(print '..::a) ; => HIERARCHICAL-PACKAGES-TEST.FOO::A

(print '...::a) ; => HIERARCHICAL-PACKAGES-TEST::A

(print '.baz::a) ; => HIERARCHICAL-PACKAGES-TEST.FOO.BAR.BAZ::A
