(in-package #:whatevs)
(in-readtable :fn.reader)

(defun symbol->ux-name (symbol &optional (qualify t))
  (format nil "~@[~a.~]~a" (when qualify (package-name (symbol-package symbol)))
          (cffi:translate-name-to-foreign symbol (symbol-package symbol))))


(defun name-to-camel (symb &optional (qualify t))
  (assert (and (symbolp symb) (symbol-package symb)))
  (labels ((str-camel (x)
             (apply #'concatenate 'string
                    (mapcar λ(format nil "~a~a" (char _ 0)
                                     (string-downcase (subseq _ 1)))
                            (split-string x :separator '(#\-))))))
    (let* ((name (str-camel (symbol-name symb)))
           (pack-name (package-name (symbol-package symb)))
           (pack-split (split-string pack-name :separator #(#\. #\-)))
           (package (format nil "~{~a~^.~}" (mapcar #'str-camel pack-split))))
      (if (or (keywordp symb) (not qualify))
          name
          (format nil "~a.~a" package name)))))
