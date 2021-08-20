;; sumibi Splash -*- lexical-binding: t; -*-

(defun show-splash ()
  (get-buffer-create "*splash*")
  (interactive)
  (with-current-buffer "*splash*"
	(erase-buffer)
    (setq header-line-format nil)
    (setq mode-line-format nil)
    (setq cursor-type nil)
    (setq line-spacing 0)
    (setq vertical-scroll-bar nil)
    (setq horizontal-scroll-bar nil)
    (face-remap-add-relative 'link :underline nil)
	(let* ((selected-img (choose-image (expand-file-name "visuals/splash-img" user-emacs-directory))))

	;; top padding
	(insert-char ?\n 5)

	;; center image horizontally with spaces
    (insert (propertize " " 'display
     					  `(space :align-to (+ center (-0.5 . ,(create-image selected-img))))))
    (insert-image (create-image selected-img)))
	(insert-char ?\n 3)

	(let* ((splash-text (show-text)))
    (insert (propertize " " 'display
    					`(space :align-to (+ center (-0.5 . ,(length splash-text))))))
	(insert splash-text))
	(insert "\n")
	;;set read only mode
	(read-only-mode t)
	;; jump to beginning of buffer
	(beginning-of-buffer)
    (goto-char 0)
    (local-set-key [t] 'sumibi-splash-kill)))

(defun choose-image (img-dir)
  (let* ((files (directory-files img-dir t "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)")))
  (elt files (random (length files)))))

(defun show-text ()
  (propertize (format-message "!!!" 'face 'sumibi-face-faded)))

(defun sumibi-splash-kill ()
  "Kill the splash screen buffer (immediately)."
  (interactive)
  (if (get-buffer "*splash*")
      (progn (message nil)
             (kill-buffer "*splash*"))))


(provide 'sumibi-splash)
