(in-package :whatevs)
(in-readtable :fn.reader)

(defvar *fuse-dir* (asdf:system-relative-pathname :whatevs "test"))

(defparameter *app-file-name* "foo.ux")

(defun write-to-ux-file (file-name ux-string)
  (let ((abs (subpathname* *fuse-dir* file-name)))
    (with-open-file (s abs
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      (format s "~a" ux-string))))

(defun comp-name-to-ux-filename (name)
  (subpathname*
   *fuse-dir*
   (concatenate 'string (regex-replace-all "\\." (symbol->ux-name name) "_")
                ".ux")))
