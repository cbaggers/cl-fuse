(in-package :fuse)
(in-readtable :fn.reader)

(defparameter *app-file-name* "foo.ux")

(defun fuse-build-dir (&optional (package *package*))
  (let ((pname (asdf/package-inferred-system::package-name-system
                (package-name package))))
    (if (eq t pname)
        (error "you cannot make a fuse app in package cl-user")
        (asdf:system-relative-pathname pname "test"))))

(defun write-to-ux-file (file-name ux-string)
  (let ((abs (subpathname* (fuse-build-dir) file-name)))
    (with-open-file (s abs
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      (format s "~a" ux-string))))

(defun write-to-js-file (file-name js-code)
  (let ((abs (subpathname* (fuse-build-dir) file-name)))
    (with-open-file (s abs
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      (format s "~a" js-code))))

(defun comp-name-to-ux-filename (name &optional (absolute t))
  (let ((name (concatenate 'string (regex-replace-all "\\." (symbol->ux-name name) "_")
                           ".ux")))
    (if absolute
        (subpathname* (fuse-build-dir) name)
        name)))

(defun func-name-to-js-filename (symb &optional (absolute t))
  (let ((name (format nil "~a_~a_func.js" (package-name (symbol-package symb))
                      (symbol-name symb))))
    (if absolute
        (subpathname* (fuse-build-dir) name)
        name)))

(defun var-name-to-js-filename (symb &optional (absolute t))
  (let ((name (format nil "~a_~a_var.js" (package-name (symbol-package symb))
                      (symbol-name symb))))
    (if absolute
        (subpathname* (fuse-build-dir) name)
        name)))
