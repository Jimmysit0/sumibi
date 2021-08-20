;;; sumibi-emacs core stuff -*- lexical-binding: t; -*-

;; Convert keyword to string without colon
(defun keyword-to-name-str (keyword)
  "Return KEYWORD symbol without initial colon as string
i.e. :keyword to \"keyword\"."
  (substring (symbol-name keyword) 1))

(defmacro lambda! (&rest body)
  "A shortcut for inline interactive lambdas."
  (declare (doc-string 1))
  `(lambda () (interactive) ,@body))

(defun sumibi/upgrade ()
  "Upgrade all installed packages using straight-x."
  (interactive)
  (progn
    (straight-x-fetch-all)
    (straight-rebuild-all)))

;; Detect system type
(defconst *IS-LINUX*   (eq system-type 'gnu/linux))
(defconst *IS-MAC*     (eq system-type 'darwin))
(defconst *IS-WINDOWS* (memq system-type '(cygwin windows-nt ms-dos)))
(defconst *IS-BSD*     (or *IS-MAC* (eq system-type 'berkeley-unix)))

(provide 'sumibi-core)

;;; sumibi-core.el ends here
