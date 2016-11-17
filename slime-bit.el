;;--------------------

(defun slime-fuse-expand (&optional repeatedly)
  "Display the macro expansion of the form starting at point.
The form is expanded with CL:MACROEXPAND-1 or, if a prefix
argument is given, with CL:MACROEXPAND.  If the form denotes a
compiler macro, SWANK/BACKEND:COMPILER-MACROEXPAND or
SWANK/BACKEND:COMPILER-MACROEXPAND-1 are used instead."
  (interactive "P")
  (slime-eval-fuse-expand 'fuse::expand-all))

(defvar slime-eval-fuse-expression nil
  "Specifies the last macroexpansion preformed.
This variable specifies both what was expanded and how.")

(defun slime-eval-fuse-expand (expander &optional string)
  (let ((string (or string (slime-sexp-at-point-or-error))))
    (setq slime-eval-fuse-expression `(,expander ,string))
    (slime-eval-async slime-eval-fuse-expression
      #'slime-initialize-fuse-expand-buffer)))

(defun slime-initialize-fuse-expand-buffer (expansion &optional buffer)
  (pop-to-buffer (or buffer (slime-create-macroexpansion-buffer)))
  (setq buffer-undo-list nil) ; Get rid of undo information from
                                        ; previous expansions.
  (let ((inhibit-read-only t)
        (buffer-undo-list t)) ; Make the initial insertion not be undoable.
    (erase-buffer)
    (insert expansion)
    (goto-char (point-min))
    (font-lock-fontify-buffer)))


;;--------------------
