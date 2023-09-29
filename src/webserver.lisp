(defpackage #:rcv
  (:use :cl))

;; C-c ~ to sync
(in-package #:rcv)

;; Load deps
(ql:quickload :clack)
(ql:quickload :ningle)

;; Describe function (K)
(defvar app (make-instance 'ningle:app))

;; Define a route
(setf (ningle:route app "/hello")
      "Yooooo")

(setf (ningle:route app "/:limit")
      "Yooooo")
(clack:clackup app)



