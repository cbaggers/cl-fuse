(in-package #:fuse)
(in-readtable :fn.reader)

;;------------------------------------------------------------

(defvar *debug* nil)

(defmacro debug-ux (&body body)
  `(let ((*debug* t))
     ,@body))

(defun fuse-expand-all (foo)
  (with-output-to-string (s)
    (let ((*standard-output* s))
      (funcall (compile nil `(lambda () (debug-ux ,foo)))))))

(swank::defslimefun expand-all (string)
  (swank::apply-macro-expander #'fuse-expand-all string))

(defmacro with-dbg-print (&body body)
  `(progn
     (when *debug* (format t "~%-----------~%"))
     (prog1 (progn ,@body)
       (when *debug* (format t "~%~%-----------~%")))))

;;------------------------------------------------------------

(defun code-to-ux (code)
  (let ((xml (toxml code :indent t)))
    (when *debug*
      (format t "~%~%~a" xml))
    xml))

;;------------------------------------------------------------

(defvar *js-funcs-used*)
(defvar *js-vars-used*)
(defvar *properties* nil)
(defvar *dependencies* nil)

(defun calc-js-files-used (vars-used funcs-used)
  (append (mapcar λ(var-name-to-js-filename _ nil) vars-used)
          (mapcar λ(func-name-to-js-filename _ nil) funcs-used)))

(defvar *preview* nil)

(defun create-app-project (name)
  (let* ((name-str (typecase name
                    (symbol (string-downcase (symbol-name name)))
                    (otherwise name)))
         (name (intern (string-upcase name-str) :keyword))
         (template (ensure-directory-pathname
                    (asdf:system-relative-pathname :fuse "project-template"))))
    (quickproject:make-project name-str :depends-on '(:fuse)
                               :template-directory template)
    (funcall (symbol-function (find-symbol "QUICKLOAD" :ql)) name-str)
    (let ((dir (fuse-build-dir name)))
      (ensure-directories-exist dir)
      (with-open-file (s (subpathname* dir *app-file-name*)
                         :direction :output
                         :if-does-not-exist :create)
        (format s "<App>~%</App>~%"))
      (with-open-file (s (subpathname* dir (format nil "~a.unoproj" name-str))
                         :direction :output
                         :if-does-not-exist :create)
        (format s "{~%  \"RootNamespace\":\"\",~%  \"Packages\": [~%      \"Fuse\",~%      \"FuseJS\"~%  ],~%  \"Includes\": [~%    \"*\"~%  ]~%}"))
      dir)))

(defun preview-app (&optional package-name)
  (let ((dir (fuse-build-dir (if package-name
                                 (find-package package-name)
                                 *package*))))
    (if (and *preview* (thread-alive-p *preview*))
        (warn "preview is already running")
        (progn
          (format t "~%starting preview: ~a" dir)
          (setf *preview* (make-thread λ(%run-preview dir) :name "Fuse-Preview"))))))

(defun %run-preview (dir)
  (with-open-stream (o (make-string-output-stream))
    (with-open-stream (eo (make-string-output-stream))
      (uiop:run-program (format nil "fuse preview ~a &" dir)
                        :output o :error-output eo))))

(defmacro def-fuse-app ((&key todo) &body body-form)
  (declare (ignore todo))
  (assert (= (length body-form) 1))
  `(%ux-app ',(first body-form)))

(defun %ux-app (body)
  (let* ((*js-vars-used* nil)
         (body (process-ux body))
         (imp-name 'app-mainview)
         (js-imports (let ((file (gen-ux-js-import-file imp-name *js-vars-used*)))
                       (when file
                         `(("JavaScript" (("File" ,(namestring file))))))))
         (code `("App" ()
                       ,@js-imports
                       ,body))
         (ux (code-to-ux code)))
    (unless *debug*
      (write-to-ux-file *app-file-name* ux))))

;;------------------------------------------------------------

(defmacro def (name args/val &body body)
  (if body
      `(def-js-func ,name ,args/val ,@body)
      `(def-js-var ,name ,args/val)))

;;------------------------------------------------------------

(defmacro def-ux-component (name properties dependencies &body body-form)
  (assert (= (length body-form) 1))
  `(%ux-comp ',name ',properties ',dependencies ',(first body-form)))

(defun %ux-comp (name properties dependencies body)
  (declare (ignore dependencies))
  (let* ((*js-vars-used* nil)
         (*properties* (mapcar #'first properties))
         (body (process-ux body))
         (imp-name (make-symbol (format nil "component-~a" name)))
         (js-imports (let ((file (gen-ux-js-import-file imp-name *js-vars-used*)))
                       (when file
                         `(("JavaScript" (("File" ,(namestring file))))))))
         (code (add-ux-class-name name body))
         (code (add-properties-to-component code properties))
         (code (add-imports-to-component code js-imports))
         (ux (code-to-ux code))
         (filename (comp-name-to-ux-filename name)))
    (if *debug*
        (format t "~%Will write to ~a" filename)
        (write-to-ux-file filename ux))))

(defun add-ux-class-name (name code)
  (let ((new-sec (cons `("ux:Class" ,(name-to-camel name)) (second code))))
    `(,(first code) ,new-sec ,@(cddr code))))

(defun add-imports-to-component (code js-imports)
  (append (subseq code 0 2)
          js-imports
          (subseq code 2)))

(defun add-properties-to-component (code properties)
  (let* ((properties (mapcar λ(dbind (name val type) _
                                `(,(name-to-camel name nil)
                                   ,val
                                   ,(name-to-camel type)))
                              properties))
         (args (mapcar λ(subseq _ 0 2) properties))
         (decls (mapcar λ(dbind (name val type) _
                           (declare (ignore val))
                           `(,type (("ux:Property" ,name))))
                        properties)))
    `(,(first code)
       ,(append (second code) args)
       ,@decls
       ,@(subseq code 2))))

;;------------------------------------------------------------

(defun process-args (args)
  (when args
    (group (mapcar λ(if (symbolp _) (process-arg-symbol _) _) args)
           2)))

(defun var-to-ref (symb)
  (format nil "{~a}" (name-to-camel symb nil)))

(defun var-to-prop (symb)
  (format nil "{Property this.~a}" (name-to-camel symb nil)))

(defun process-arg-symbol (symb)
  (cond
    ((find symb *properties*) (var-to-prop symb))
    ((find symb *js-vars*) (pushnew symb *js-vars-used*) (var-to-ref symb))
    (t (name-to-camel symb))))

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
  `(defparameter ,(intern "*PACKAGE-JS-REQUIRES*" *package*)
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

(defparameter *js-funcs* nil)
(defparameter *js-vars* nil)

(defun notice-js-var-name (name)
  (pushnew name *js-vars*)
  name)

(defun forget-js-var-name (name)
  (setf *js-vars* (remove name *js-vars*))
  name)

(defun notice-js-func-name (name)
  (pushnew name *js-funcs*)
  name)

(defun forget-js-func-name (name)
  (setf *js-funcs* (remove name *js-funcs*))
  name)

(defun sweep-for-vars (code)
  (remove-duplicates
   (remove-if-not λ(find _ *js-vars*) (flatten code))))

(defun sweep-for-funcs (code)
  (remove-duplicates
   (remove-if-not λ(find _ *js-funcs*) (flatten code))))

(defun gen-js-requires (code package-requires)
  (%gen-js-requires (sweep-for-funcs code)
                    (sweep-for-vars code)
                    package-requires))

(defun %gen-js-requires (funcs vars package-requires)
  (labels ((gen-req (var is-func?)
             (let* ((file (if is-func?
                              (func-name-to-js-filename var nil)
                              (var-name-to-js-filename var nil)))
                    (symb (make-symbol (name-to-camel var nil))))
               `(defvar ,symb (require ,file)))))
    (values
     (append (mapcar λ(dbind (req var) _ `(defvar ,var (require ,req)))
                     package-requires)
             (mapcar λ(gen-req _ t) funcs)
             (mapcar λ(gen-req _ nil) vars))
     (mapcar λ(list _ `(-> ,(make-symbol (name-to-camel _ nil))
                           ,(name-to-camel _ nil)))
             (append funcs vars)))))

(defun gen-ux-js-import-file (name vars)
  (when vars
    (let* ((filename (var-name-to-js-filename name nil))
           (abs-file-name (var-name-to-js-filename name t))
           (reqs (%gen-js-requires nil vars nil))
           (js-code
            (parenscript:ps*
             `(progn
                ,@reqs
                ,@(mapcar
                   λ(let ((var (second _)))
                      `(setf (parenscript:chain module exports ,var)
                             (parenscript:chain ,var ,var)))
                   reqs)))))
      (if *debug*
          (format t "~%~a~%~%Will write to ~a" js-code abs-file-name)
          (write-to-js-file abs-file-name js-code))
      filename)))

;;------------------------------------------------------------

(defmacro def-js-func (name args &body body)
  (let ((req (find-symbol "*PACKAGE-JS-REQUIRES*" (symbol-package name))))
    (unless *debug* (notice-js-func-name name))
    `(%def-js-func ',name ',args ',body ,req)))

(defun gen-js-func-code (name args body requires)
  (vbind (reqs macros) (gen-js-requires body requires)
    `(progn
       ,@reqs
       (defun ,name ,args
         (symbol-macrolet ,macros
           ,@body))
       (setf (parenscript:chain module exports ,(name-to-camel name nil)) ,name))))

(defun %def-js-func (name args body requires)
  (let* ((js-code (parenscript:ps* (gen-js-func-code name args body requires)))
         (filename (func-name-to-js-filename name)))
    (if *debug*
        (format t "~%~a~%~%Will write to ~a" js-code filename)
        (write-to-js-file filename js-code))))

;;------------------------------------------------------------



(defmacro def-js-var (name form)
  (if form
      (let ((req (find-symbol "*PACKAGE-JS-REQUIRES*" (symbol-package name))))
        (unless *debug* (notice-js-var-name name))
        `(%def-js-var ',name ',form ,req))
      (unless *debug*
        (forget-js-var-name name))))

(defun gen-js-var-code (name form requires)
  (vbind (reqs macros) (gen-js-requires form requires)
    `(progn
       ,@reqs
       (symbol-macrolet ,macros
         (setf (parenscript:chain module exports ,(name-to-camel name nil))
               ,form)))))


(defun %def-js-var (name form requires)
  (let* ((js-code (parenscript:ps* (gen-js-var-code name form requires)))
         (filename (var-name-to-js-filename name)))
    (if *debug*
        (format t "~%~a~%~%Will write to ~a" js-code filename)
        (write-to-js-file filename js-code))))

(parenscript:defpsmacro -> (&rest args)
  `(parenscript:getprop ,@args))
