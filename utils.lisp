(in-package #:fuse)
(in-readtable :fn.reader)

(defun symbol->ux-name (symbol &optional (qualify t))
  (format nil "~@[~a.~]~a" (when qualify (package-name (symbol-package symbol)))
          (cffi:translate-name-to-foreign symbol (symbol-package symbol))))


(defun name-to-camel (symb &optional (qualify t))
  (assert (symbolp symb))
  ;;(format t "~%~a:~a" (package-name (symbol-package symb)) symb)
  (when (find symb fuse.controls::std-lib-macro-map :key #'first)
    (setf qualify nil))
  (or (second (find symb fuse.controls::std-lib-name-map :key #'first))
      (labels ((str-camel (x)
                 (apply #'concatenate 'string
                        (mapcar λ(format nil "~a~a" (char _ 0)
                                         (string-downcase (subseq _ 1)))
                                (split-string x :separator '(#\-))))))
        (let* ((name (str-camel (symbol-name symb))))
          (if (or (keywordp symb) (not qualify))
              name
              (let* ((pack-name (package-name (symbol-package symb)))
                     (pack-split (split-string pack-name :separator #(#\. #\-)))
                     (package (format nil "~{~a~^.~}"
                                      (mapcar #'str-camel pack-split))))
               (format nil "~a.~a" package name)))))))

(defmacro dbind (lambda-list expression &body body)
  `(destructuring-bind ,lambda-list ,expression ,@body))

(defmacro vbind (vars value-form &body body)
  ;; {TODO} handle declare forms properly. It is complicated
  ;;        as the declare has to be the first thing in the scope
  ;;        but the vars are now split across multiple binds
  (let* ((list? (mapcar #'listp vars))
	 (mvb-vars (mapcar (lambda (v l?) (if l? (gensym) v)) vars list?))
	 (d-vars (mapcar (lambda (v l?) (when l? v)) vars list?))
	 (d-forms (mapcar (lambda (mvb d)
			    (when d `(dbind ,d ,mvb)))
			  mvb-vars d-vars))
	 (d-forms (remove nil d-forms)))
    `(multiple-value-bind ,mvb-vars ,value-form
       ,@(reduce (lambda (accum x)
		   (list (append x accum)))
		 (cons body d-forms)))))


(defun group (source n)
  "This takes a  flat list and emit a list of lists, each n long
   containing the elements of the original list"
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
	     (let ((rest (nthcdr n source)))
	       (if (consp rest)
		   (rec rest (cons (subseq source 0 n)
				   acc))
		   (nreverse (cons source acc))))))
    (if source
	(rec source nil)
	nil)))

(defun n-of (n x)
  (loop :for i :below n :collect x))
