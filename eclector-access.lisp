(defpackage #:eclector-access
  (:use #:cl)
  (:export #:enable
           #:client
           #:client-readtable
           #:translate-read-result))

(in-package #:eclector-access)

(defvar *standard-characters*
  '(#\Newline #\Space
    #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
    #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
    #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
    #\! #\" #\# #\$ #\% #\& #\' #\( #\) #\* #\+ #\, #\- #\. #\/ #\: #\; #\< #\= #\> #\? #\@
    #\[ #\\ #\] #\^ #\_ #\` #\{ #\| #\} #\~))

;;; The :character-set initarg should be used to supply the character
;;; set with which the readtable will be primed. By default,
;;; *standard-characters* will be used. Whenever a character in the
;;; set is encountered, Eclector will be used to read from the stream
;;; instead of the standard reader.

(defclass client ()
  ((%readtable :initarg :readtable
               :initform (copy-readtable nil)
               :reader access-readtable
               :reader client-readtable)
   (%character-set :initarg :character-set
                   :initform *standard-characters*
                   :reader character-set)))

(defgeneric translate-read-result (client read-result)
  (:method (client read-result)
    read-result))

(defmethod initialize-instance :after ((client client) &key)
  (let ((reader-macro-function (lambda (stream char)
                                 (unread-char char stream)
                                 (let* ((eclector.reader:*client* client)
                                        (result (eclector.reader:read stream
                                                                      nil
                                                                      stream)))
                                   (if (eq result stream)
                                       (values)
                                       (translate-read-result client result))))))
    (dolist (char (character-set client))
      (set-macro-character char
                           reader-macro-function
                           nil
                           (access-readtable client)))))

(defmacro enable (client)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf *readtable* (access-readtable ,client))))
