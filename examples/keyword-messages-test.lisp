(defpackage #:keyword-messages-test
  (:use #:cl))

(in-package #:keyword-messages-test)

;;; This file demonstrates how to use the keyword messages defined
;;; keyword-messages.lisp. Please see that file for a more detailed
;;; explanation.
;;;
;;; If you're using Emacs, try this:
;;;
;;; Select the following expressionand do M-x eval-region to highlight
;;; the new message keywords in a fresh green color :)
;;;
;;; (font-lock-add-keywords nil '((" \\(\\(\\sw\\|\\s_\\)+\\):" (0 '(t :foreground "Aquamarine")))))
;;;
;;; (don't worry, its buffer-local)

(eclector-access:enable (make-instance 'keyword-messages:reader
                                       :marker "="))

;;; Let's define some "messages".

(defgeneric to= (self to))
(defgeneric to=do= (self to do))

(defmethod to= ((self number) number)
  (loop :for k :from self :to number
        :collect k))

(defmethod to=do= ((self number) number function)
  (loop :for k :from self :to number
        :do (funcall function k)))

(defun squared (self)
  (* self self))

;;; And now let's "send" them.

(print '(1 to: 5)) ; => (TO= 1 5)

((1 to: 5) print:) ; => (1 2 3 4 5)

(print '(1 to: 5 do: (lambda (n) (n squared: print:))))

; => (TO=DO= 1 5 (LAMBDA (N) (PRINT (SQUARED N))))

(1 to: 5 do: (lambda (n) (n squared: print:)))

; => 1
;    4
;    9
;    16
;    25
