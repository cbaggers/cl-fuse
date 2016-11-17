;;;; fuse.asd

(asdf:defsystem #:fuse
  :description "Generate Fuse Apps from lisp"
  :author "Chris Bagley (Baggers) <techsnuffle@gmail.com>"
  :license "GPLv3"
  :depends-on (:alexandria :parenscript :xmls :uiop :fn :cffi :cl-ppcre
                           :bordeaux-threads :quickproject)
  :serial t
  :components ((:file "package")
               (:file "std-lib")
               (:file "utils")
               (:file "file-shiz")
               (:file "base")))
