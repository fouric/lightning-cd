;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:lightning-cd-asd
  (:use :cl :asdf))

(in-package :lightning-cd-asd)

(defsystem lightning-cd
  :name "lightning-cd"
  :version "0.0.0"
  :maintainer "fouric"
  :author "fouric"
  :license "All rights reserved"
  :description "lightning-quick mc/cd/autojump/rofi hybrid "

  :serial t
  :depends-on (:split-sequence :trivial-shell :cl-charms :alexandria :fouric)
  :pathname "src"
  :components ((:file "package")
               (:file "lightning-cd")))
