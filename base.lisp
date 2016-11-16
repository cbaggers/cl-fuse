(in-package #:whatevs)
(in-readtable :fn.reader)

;;------------------------------------------------------------

(defvar *debug* nil)

(defmacro debug-ux (&body body)
  `(let ((*debug* t))
     ,@body))

(defmacro with-dbg-print (&body body)
  `(progn
     (when *debug* (format t "~%-----------~%"))
     (prog1 (progn ,@body)
       (when *debug* (format t "~%~%-----------~%")))))

;;------------------------------------------------------------

(defun code-to-ux (code)
  (when *debug*
    (print code))
  (let ((xml (toxml code :indent t)))
    (when *debug*
      (format t "~%~%~a" xml))
    xml))

;;------------------------------------------------------------

(defvar *js-funcs-used*)
(defvar *js-vars-used*)

(defun calc-js-files-used (vars-used funcs-used)
  (append (mapcar λ(var-name-to-js-filename _ nil) vars-used)
          (mapcar λ(func-name-to-js-filename _ nil) funcs-used)))

(defmacro def-fuse-app ((&key todo) &body body-form)
  (declare (ignore todo))
  (assert (= (length body-form) 1))
  `(%ux-app ',(first body-form)))

(defun %ux-app (body)
  (with-dbg-print
    (let* ((*js-vars-used* nil)
           (*js-funcs-used* nil)
           (body (process-ux body))
           (js-imports (let ((files (calc-js-files-used *js-vars-used* *js-funcs-used*)))
                         (mapcar λ`("JavaScript" (("File" ,(namestring _)))) files)))
           (code `("App" ()
                         ,@js-imports
                         ,body))
           (ux (code-to-ux code)))
      (unless *debug*
        (write-to-ux-file *app-file-name* ux)))))

;;------------------------------------------------------------

(defmacro def-ux-component (name properties dependencies &body body-form)
  (assert (= (length body-form) 1))
  `(%ux-comp ',name ',properties ',dependencies ',(first body-form)))

(defun %ux-comp (name properties dependencies body)
  (declare (ignore properties dependencies))
  (with-dbg-print
    (let* ((*js-vars-used* nil)
           (*js-funcs-used* nil)
           (body (process-ux body))
           (js-imports (let ((files (calc-js-files-used *js-vars-used* *js-funcs-used*)))
                         (mapcar λ`("JavaScript" (("File" ,(namestring _)))) files)))
           (code (add-ux-class-name name body))
           (code (add-imports-to-component code js-imports))
           (ux (code-to-ux code))
           (filename (comp-name-to-ux-filename name)))
      (if *debug*
          (format t "~%Will write to ~a" filename)
          (write-to-ux-file filename ux)))))

(defun add-ux-class-name (name code)
  (let ((new-sec (cons `("ux:Class" ,(name-to-camel name)) (second code))))
    `(,(first code) ,new-sec ,@(cddr code))))

(defun add-imports-to-component (code js-imports)
  (append (subseq code 0 2)
          js-imports
          (subseq code 2)))

;;------------------------------------------------------------

(defun process-args (args)
  (when args
    (group (mapcar λ(if (symbolp _) (process-arg-symbol _) _) args)
           2)))

(defun var-to-ref (symb)
  (format nil "{~a}" (name-to-camel symb nil)))

(defun process-arg-symbol (symb)
  (if (find symb *js-vars*)
      (progn
        (pushnew symb *js-vars-used*)
        (var-to-ref symb))
      (name-to-camel symb)))

(defmethod process-ux ((code list))
  (let* ((head (first code))
         (name (etypecase head
                 (string head)
                 (symbol (name-to-camel (first code))))))
    `(,name
       ,(process-args (second code))
       ,@(mapcar #'process-ux (cddr code)))))

(defmethod process-ux ((code symbol))
  ;; (if (find code *js-vars*)
  ;;     (var-to-ref code)
  ;;     code)
  code)

(defmethod process-ux (code)
  code)

;;------------------------------------------------------------

(defmacro def-js-requires (&body requires)
  `(defparameter *package-js-requires*
     ',(mapcar λ(destructuring-bind (req var-name) _
                  (assert (or (symbolp req) (stringp req)))
                  (assert (symbolp var-name))
                  (assert (not (keywordp var-name)))
                  (list (if (stringp req)
                            req
                            (name-to-camel req))
                        var-name))
               requires)))

;;------------------------------------------------------------

(defmacro def-js-func (name args &body body)
  (let ((req (find-symbol "*PACKAGE-JS-REQUIRES*" (symbol-package name))))
    `(%def-js-func ',name ',args ',body ,req)))

(defun gen-js-func-code (name args body requires)
  `(progn
     ,@(mapcar λ(dbind (req var) _ `(defvar ,var (require ,req)))
               requires)
     (defun ,name ,args ,@body)
     (setf (parenscript:chain module exports ,(name-to-camel name nil)) ,name)))


(defun %def-js-func (name args body requires)
  (let* ((js-code (parenscript:ps* (gen-js-func-code name args body requires)))
         (filename (func-name-to-js-filename name)))
    (if *debug*
        (with-dbg-print
          (format t "~%~a~%~%Will write to ~a" js-code filename))
        (write-to-js-file filename js-code))))

;;------------------------------------------------------------

(defparameter *js-vars* nil)

(defun notice-js-var-name (name)
  (pushnew name *js-vars*)
  name)

(defun forget-js-var-name (name)
  (setf *js-vars* (remove name *js-vars*))
  name)

(defmacro def-js-var (name form)
  (if form
      (let ((req (find-symbol "*PACKAGE-JS-REQUIRES*" (symbol-package name))))
        (unless *debug*
          (notice-js-var-name name))
        `(%def-js-var ',name ',form ,req))
      (unless *debug*
        (forget-js-var-name name))))

(defun gen-js-var-code (name form requires)
  `(progn
     ,@(mapcar λ(dbind (req var) _
                  `(defvar ,var (require ,req))) requires)
     (setf (parenscript:chain module exports ,(name-to-camel name nil)) ,form)))


(defun %def-js-var (name form requires)
  (let* ((js-code (parenscript:ps* (gen-js-var-code name form requires)))
         (filename (var-name-to-js-filename name)))
    (if *debug*
        (with-dbg-print
          (format t "~%~a~%~%Will write to ~a" js-code filename))
        (write-to-js-file filename js-code))))
