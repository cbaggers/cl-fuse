;;;; whatevs.asd

(asdf:defsystem #:whatevs
  :description "Generate Fuse Apps from lisp"
  :author "Chris Bagley (Baggers) <techsnuffle@gmail.com>"
  :license "GPLv3"
  :depends-on (:alexandria :parenscript :xmls :uiop :fn :cffi :cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "file-shiz")
               (:file "base")))
