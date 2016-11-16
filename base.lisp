(in-package #:whatevs)
(in-readtable :fn.reader)

(defvar *debug* nil)

(defmacro debug-ux (&body body)
  `(let ((*debug* t))
     ,@body))

;;------------------------------------------------------------

(defun code-to-ux (code)
  (when *debug*
    (print code))
  (let ((xml (toxml code :indent t)))
    (when *debug*
      (format t "~%~%~a" xml))
    xml))

;;------------------------------------------------------------


(defmacro ux-app ((&key todo) &body body-form)
  (declare (ignore todo))
  (assert (= (length body-form) 1))
  `(%ux-app ',(first body-form)))

(defun %ux-app (body)
  (when *debug* (format t "~%-----------~%"))
  (let* ((body (process-ux body))
         (code `("App" () ,body))
         (ux (code-to-ux code)))
    (unless *debug*
      (write-to-ux-file *app-file-name* ux)))
  (when *debug* (format t "~%~%-----------~%"))
  nil)

;;------------------------------------------------------------

(defmacro ux-comp (name properties dependencies &body body-form)
  (assert (= (length body-form) 1))
  `(%ux-comp ',name ',properties ',dependencies ',(first body-form)))

(defun %ux-comp (name properties dependencies body)
  (declare (ignore properties dependencies))
  (when *debug* (format t "~%-----------~%"))
  (let* ((body (process-ux body))
         (code (add-ux-class-name name body))
         (ux (code-to-ux code))
         (filename (comp-name-to-ux-filename name)))
    (if *debug*
        (format t "~%Will write to ~a" filename)
        (write-to-ux-file filename ux)))
  (when *debug* (format t "~%~%-----------~%"))
  name)

(defun add-ux-class-name (name code)
  (let ((new-sec (cons `("ux:Class" ,(name-to-camel name)) (second code))))
    `(,(first code) ,new-sec ,@(cddr code))))

;;------------------------------------------------------------

(defun process-args (args)
  (when args
    (list (mapcar λ(if (symbolp _) (name-to-camel _) _) args))))

(defmethod process-ux ((code list))
  (let* ((head (first code))
         (name (etypecase head
                 (string head)
                 (symbol (name-to-camel (first code))))))
    `(,name
       ,(process-args (second code))
       ,@(mapcar #'process-ux (cddr code)))))

(defmethod process-ux (code)
  code)

;;------------------------------------------------------------

(debug-ux)
(ux-app ()
  (stack-panel ()
    (text (:color :blue) "Hi there. This is a dumb project")
    (text (:color :red) "testing")
    (text (:color :red) "yip!")))

(debug-ux
  (ux-comp my-comp () ()
    (panel (:color "Yellow")
      (while-pressed ()
        (scale (:factor 2 :duration 0.3 :easing :back-out))))))
