;; sumibi-modeline -*- lexical-binding: t -*-
;; -------------------------------------------------------------------
;;
;; mode line format:
;;
;; [ status | name (primary)               secondary | item1 | item2 ]
;;
;; -------------------------------------------------------------------
(require 'subr-x)


;; -------------------------------------------------------------------
(defun vc-branch ()
  (if vc-mode
      (let ((backend (vc-backend buffer-file-name)))
        (concat "#" (substring-no-properties vc-mode
                                 (+ (if (eq backend 'Hg) 2 3) 2))))  nil))

(defun sumibi-mode-name ()
  (if (listp mode-name) (car mode-name) mode-name))


;; From https://amitp.blogspot.com/2011/08/emacs-custom-mode-line.html
;; ---------------------------------------------------------------------
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat "…/" output)))
    output))

;; -------------------------------------------------------------------
(defun sumibi-modeline-compose (status name primary secondary)
  "Compose a string with provided information"
  (let* ((char-width    (window-font-width nil 'header-line))
         (window        (get-buffer-window (current-buffer)))
         (space-up       +0.15)
         (space-down     -0.20)
	 (prefix (cond ((string= status "RO")
			        (propertize (if (window-dedicated-p)" -- " " RO ")
                                'face 'sumibi-face-header-popout))
                   ((string= status "OH")
			        (propertize (if (window-dedicated-p)" -- " " OH ")
                                'face 'sumibi-face-header-critical))
                   ((string= status "OK")
			        (propertize (if (window-dedicated-p)" -- " " OK ")
                                'face 'sumibi-face-header-faded))
                   (t (propertize status 'face 'sumibi-face-header-popout))))
         (left (concat
                (propertize " "  'face 'sumibi-face-header-default
			    'display `(raise ,space-up))
                (propertize name 'face 'sumibi-face-header-strong)
                (propertize " "  'face 'sumibi-face-header-default
			    'display `(raise ,space-down))
		(propertize primary 'face 'sumibi-face-header-default)))
         (right (concat secondary " "))
         (available-width (- (window-total-width) 
			     (length prefix) (length left) (length right)
			     (/ (window-right-divider-width) char-width)))
	 (available-width (max 1 available-width)))
    (concat prefix
	    left
	    (propertize (make-string available-width ?\ )
                        'face 'sumibi-face-header-default)
	    (propertize right 'face `(:inherit sumibi-face-header-default
                                      :foreground ,sumibi-color-faded)))))

;; ---------------------------------------------------------------------
(defun sumibi-modeline-mu4e-dashboard-mode-p ()
  (bound-and-true-p mu4e-dashboard-mode))

(defun sumibi-modeline-mu4e-dashboard-mode ()
  (sumibi-modeline-compose (sumibi-modeline-status)
                         "Mail"
                         (sumibi-modeline-mu4e-context)
                         (format "%d messages" (plist-get mu4e~server-props :doccount))
                         ))

;; ---------------------------------------------------------------------

;; since the EIN library itself is constantly re-rendering the notebook, and thus
;; re-setting the header-line-format, we cannot use the sumibi-modeline function to set
;; the header format in a notebook buffer.  Fortunately, EIN exposes the
;; ein:header-line-format variable for just this purpose.

(with-eval-after-load 'ein
  (defun sumibi-modeline-ein-notebook-mode ()
    (let ((buffer-name (format-mode-line "%b")))
      (sumibi-modeline-compose (if (ein:notebook-modified-p) "OH" "OK")
                             buffer-name
                             ""
                             (ein:header-line))))
  (setq ein:header-line-format '((:eval (sumibi-modeline-ein-notebook-mode)))))

;; ---------------------------------------------------------------------
(defun sumibi-modeline-elfeed-search-mode-p ()
  (derived-mode-p 'elfeed-search-mode))

(defun sumibi-modeline-elfeed-search-mode ()
  (sumibi-modeline-compose (sumibi-modeline-status)
                         "Elfeed"
                         (concat "(" (elfeed-search--header)  ")")
                         ""))

;; Elfeed (regular header)
(with-eval-after-load 'elfeed
  (defun elfeed-setup-header ()
    (setq header-line-format (default-value 'header-line-format)))
  (setq elfeed-search-header-function #'elfeed-setup-header))

;; ---------------------------------------------------------------------
(defun sumibi-modeline-elfeed-show-mode-p ()
  (derived-mode-p 'elfeed-show-mode))

(defun sumibi-modeline-elfeed-show-mode ()
  (let* ((title        (elfeed-entry-title elfeed-show-entry))
         (tags         (elfeed-entry-tags elfeed-show-entry))
         (tags-str     (mapconcat #'symbol-name tags ", "))
         (date         (seconds-to-time (elfeed-entry-date elfeed-show-entry)))
         (feed         (elfeed-entry-feed elfeed-show-entry))
         (feed-title   (plist-get (elfeed-feed-meta feed) :title))
         (entry-author (elfeed-meta elfeed-show-entry :author)))
    (sumibi-modeline-compose (sumibi-modeline-status)
                           (s-truncate 40 title "…")
                           (concat "(" tags-str ")")
                           feed-title)))

;; ---------------------------------------------------------------------
(defun sumibi-modeline-calendar-mode-p ()
  (derived-mode-p 'calendar-mode))

(defun sumibi-modeline-calendar-mode () "")

;; Calendar (no header, only overline)
(with-eval-after-load 'calendar
  (defun calendar-setup-header ()
    (setq header-line-format "")
    (face-remap-add-relative
     'header-line `(:overline ,(face-foreground 'default)
                    :height 0.5
                    :background ,(face-background 'default))))
  (add-hook 'calendar-initial-window-hook #'calendar-setup-header)

  ;; From https://emacs.stackexchange.com/questions/45650
  (add-to-list 'display-buffer-alist
               `(,(rx string-start "*Calendar*" string-end)
                 (display-buffer-below-selected))))

;; ---------------------------------------------------------------------
(defun sumibi-modeline-org-capture-mode-p ()
  (bound-and-true-p org-capture-mode))

(defun sumibi-modeline-org-capture-mode ()
  (sumibi-modeline-compose (sumibi-modeline-status)
                         "Capture"
                         "(org)"
                         ""))

(with-eval-after-load 'org-capture
  (defun org-capture-turn-off-header-line ()
    (setq-local header-line-format (default-value 'header-line-format))
    ;; (fit-window-to-buffer nil nil 8)
    ;; (face-remap-add-relative 'header-line '(:background "#ffffff"))
    (message nil))
  (add-hook 'org-capture-mode-hook
            #'org-capture-turn-off-header-line))

;; ---------------------------------------------------------------------
(setq Info-use-header-line nil)
(defun sumibi-modeline-info-breadcrumbs ()
  (let ((nodes (Info-toc-nodes Info-current-file))
        (cnode Info-current-node)
	(node Info-current-node)
        (crumbs ())
        (depth Info-breadcrumbs-depth)
	line)
    (while  (> depth 0)
      (setq node (nth 1 (assoc node nodes)))
      (if node (push node crumbs))
      (setq depth (1- depth)))
    (setq crumbs (cons "Top" (if (member (pop crumbs) '(nil "Top"))
			         crumbs (cons nil crumbs))))
    (forward-line 1)
    (dolist (node crumbs)
      (let ((text
	     (if (not (equal node "Top")) node
	       (format "%s"
		       (if (stringp Info-current-file)
			   (file-name-sans-extension
			    (file-name-nondirectory Info-current-file))
			 Info-current-file)))))
	(setq line (concat line (if (null line) "" " > ")
                                (if (null node) "..." text)))))
    (if (and cnode (not (equal cnode "Top")))
        (setq line (concat line (if (null line) "" " > ") cnode)))
    line))

(defun sumibi-modeline-info-mode-p ()
  (derived-mode-p 'Info-mode))

(defun sumibi-modeline-info-mode ()
  (sumibi-modeline-compose (sumibi-modeline-status)
                         "Info"
                         (concat "("
                                 (sumibi-modeline-info-breadcrumbs)
                                 ")")
                         ""))

;; ---------------------------------------------------------------------
(defun sumibi-modeline-org-agenda-mode-p ()
  (derived-mode-p 'org-agenda-mode))

(defun sumibi-modeline-org-agenda-mode ()
  (sumibi-modeline-compose (sumibi-modeline-status)
                         "Agenda"
                         ""
                         (format-time-string "%A %-e %B %Y")))

;; ---------------------------------------------------------------------
(defun sumibi-modeline-term-mode-p ()
  (derived-mode-p 'term-mode))

(defun sumibi-modeline-vterm-mode-p ()
  (derived-mode-p 'vterm-mode))

(defun sumibi-modeline-term-mode ()
  (sumibi-modeline-compose " >_ "
                         "Terminal"
                         (concat "(" shell-file-name ")")
                         (shorten-directory default-directory 32)))

;; ---------------------------------------------------------------------
(defun sumibi-modeline-mu4e-main-mode-p ()
  (derived-mode-p 'mu4e-main-mode))

(defun sumibi-modeline-mu4e-main-mode ()
  (sumibi-modeline-compose (sumibi-modeline-status)
                         "Mail"
                         (sumibi-modeline-mu4e-context)
                         (format-time-string "%A %d %B %Y, %H:%M")))

;; ---------------------------------------------------------------------
(defun sumibi-modeline-mu4e-headers-mode-p ()
  (derived-mode-p 'mu4e-headers-mode))

(defun sumibi-modeline-mu4e-headers-mode ()
  (sumibi-modeline-compose (sumibi-modeline-status)
                         (mu4e~quote-for-modeline mu4e~headers-last-query)
                         ""
                         ""))

(with-eval-after-load 'mu4e
  (defun mu4e~header-line-format () (sumibi-modeline)))

;; ---------------------------------------------------------------------
(setq mu4e-modeline-max-width 72)

(defun sumibi-modeline-mu4e-view-mode-p ()
  (derived-mode-p 'mu4e-view-mode))

(defun sumibi-modeline-mu4e-view-mode ()
  (let* ((msg     (mu4e-message-at-point))
         (subject (mu4e-message-field msg :subject))
         (from    (mu4e~headers-contact-str (mu4e-message-field msg :from)))
         (date     (mu4e-message-field msg :date)))
    (sumibi-modeline-compose (sumibi-modeline-status)
                           (s-truncate 40 subject "…")
                           ""
                           from)))

(defun sumibi-modeline-mu4e-view-hook ()
  (setq header-line-format "%-")
  (face-remap-add-relative 'header-line
                           '(:background "#ffffff"
                                         :underline nil
                                         :box nil
                                         :height 1.0)))
(add-hook 'mu4e-view-mode-hook #'sumibi-modeline-mu4e-view-hook)


;; ---------------------------------------------------------------------
(defun sumibi-modeline-sumibi-help-mode-p ()
  (derived-mode-p 'sumibi-help-mode))

(defun sumibi-modeline-sumibi-help-mode ()
  (sumibi-modeline-compose (sumibi-modeline-status)
                         "GNU Emacs / N Λ N O"
                         "(help)"
                         ""))

;; ---------------------------------------------------------------------
(defun sumibi-modeline-message-mode-p ()
  (derived-mode-p 'message-mode))

(defun sumibi-modeline-message-mode ()
  (sumibi-modeline-compose (sumibi-modeline-status)
                         "Message" "(draft)" ""))


;; ---------------------------------------------------------------------
(setq org-mode-line-string nil)
(with-eval-after-load 'org-clock
  (add-hook 'org-clock-out-hook
            '(lambda () (setq org-mode-line-string nil)
                        (force-mode-line-update))))

(defun sumibi-modeline-org-clock-mode-p ()
  org-mode-line-string)

(defun sumibi-modeline-org-clock-mode ()
    (let ((buffer-name (format-mode-line "%b"))
          (mode-name   (sumibi-mode-name))
          (branch      (vc-branch))
          (position    (format-mode-line "%l:%c")))
      (sumibi-modeline-compose (sumibi-modeline-status)
                             buffer-name 
                             (concat "(" mode-name
                                     (if branch (concat ", "
                                             (propertize branch 'face 'italic)))
                                     ")" )
                             org-mode-line-string)))

;; ---------------------------------------------------------------------
(defun sumibi-modeline-docview-mode-p ()
  (derived-mode-p 'doc-view-mode))

(defun sumibi-modeline-docview-mode ()
  (let ((buffer-name (format-mode-line "%b"))
	(mode-name   (sumibi-mode-name))
	(branch      (vc-branch))
	(page-number (concat
		      (number-to-string (doc-view-current-page)) "/"
		      (or (ignore-errors
			    (number-to-string (doc-view-last-page-number)))
			  "???"))))
    (sumibi-modeline-compose
     (sumibi-modeline-status)
     buffer-name
     (concat "(" mode-name
	     (if branch (concat ", "
				(propertize branch 'face 'italic)))
	     ")" )
     page-number)))

;; ---------------------------------------------------------------------
(defun sumibi-modeline-pdf-view-mode-p ()
  (derived-mode-p 'pdf-view-mode))

(defun sumibi-modeline-pdf-view-mode ()
  (let ((buffer-name (format-mode-line "%b"))
	(mode-name   (sumibi-mode-name))
	(branch      (vc-branch))
	(page-number (concat
		      (number-to-string (pdf-view-current-page)) "/"
		      (or (ignore-errors
			    (number-to-string (pdf-cache-number-of-pages)))
			  "???"))))
    (sumibi-modeline-compose
     "OK"
     buffer-name
     (concat "(" mode-name
	     (if branch (concat ", "
				(propertize branch 'face 'italic)))
	     ")" )
     page-number)))

;; ---------------------------------------------------------------------
(defun buffer-menu-mode-header-line ()
  (face-remap-add-relative
   'header-line `(:background ,(face-background 'sumibi-face-subtle))))
(add-hook 'Buffer-menu-mode-hook
          #'buffer-menu-mode-header-line)

;; ---------------------------------------------------------------------
(defun sumibi-modeline-completion-list-mode-p ()
  (derived-mode-p 'completion-list-mode))

(defun sumibi-modeline-completion-list-mode ()
    (let ((buffer-name (format-mode-line "%b"))
          (mode-name   (sumibi-mode-name))
          (position    (format-mode-line "%l:%c")))

      (sumibi-modeline-compose (sumibi-modeline-status)
                             buffer-name "" position)))
;; ---------------------------------------------------------------------
(with-eval-after-load 'deft
  (defun deft-print-header ()
    (force-mode-line-update)
    (widget-insert "\n")))

(defun sumibi-modeline-deft-mode-p ()
  (derived-mode-p 'deft-mode))

(defun sumibi-modeline-deft-mode ()
  (let ((prefix " DEFT ")
        (primary "Notes")
        (filter  (if deft-filter-regexp
                     (deft-whole-filter-regexp) "<filter>"))
        (matches (if deft-filter-regexp
                     (format "%d matches" (length deft-current-files))
                   (format "%d notes" (length deft-all-files)))))
    (sumibi-modeline-compose " DEFT "
                           primary filter matches)))
    
;; ---------------------------------------------------------------------
(defun sumibi-modeline-prog-mode-p ()
  (derived-mode-p 'prog-mode))

(defun sumibi-modeline-text-mode-p ()
  (derived-mode-p 'text-mode))

;; ---------------------------------------------------------------------
(defun sumibi-modeline-flycheck-count-all ()
  (let ((info 0) (warning 0) (error 0))
    (mapc
     (lambda (item)
       (let ((count (cdr item)))
	 (pcase (flycheck-error-level-compilation-level (car item))
	   (0 (cl-incf info count))
	   (1 (cl-incf warning count))
	   (2 (cl-incf error count)))))
     (flycheck-count-errors flycheck-current-errors))
    `((info . ,info) (warning . ,warning) (error . ,error))))

(defun sumibi-modeline-flycheck-print-counts ()
  (let-alist (sumibi-modeline-flycheck-count-all)
    (format "ERR %s | WARN %s | INFO %s"
	    (number-to-string .error)
	    (number-to-string .warning)
	    (number-to-string .info))))

;; ---------------------------------------------------------------------
(defun sumibi-modeline-default-mode ()
    (let ((buffer-name (format-mode-line "%b"))
          (mode-name   (sumibi-mode-name))
          (branch      (vc-branch))
          (position    (format-mode-line "%l:%c")))
      (sumibi-modeline-compose (sumibi-modeline-status)
                             buffer-name
                             (concat "(" mode-name
                                     (if branch (concat ", "
                                            (propertize branch 'face 'italic)))
                                     ")" )
                             (concat (sumibi-modeline-flycheck-print-counts) " | "  position))))

;; ---------------------------------------------------------------------
(defun sumibi-modeline-status ()
  "Return buffer status: read-only (RO), modified (OH) or read-write (OK)"
  
  (let ((read-only   buffer-read-only)
        (modified    (and buffer-file-name (buffer-modified-p))))
    (cond (modified  "OH") (read-only "RO") (t "OK"))))
  
;; ---------------------------------------------------------------------
(defun sumibi-modeline-mu4e-context ()
  "Return the current mu4e context as a non propertized string."

  (if (> (length (mu4e-context-label)) 0)
      (concat "(" (substring-no-properties (mu4e-context-label) 1 -1) ")")
    "(none)"))


;; ---------------------------------------------------------------------
(defun sumibi-modeline ()
  "Install a header line whose content is dependend on the major mode"
  (interactive)
  (setq-default header-line-format
  '((:eval
     (cond ((sumibi-modeline-prog-mode-p)            (sumibi-modeline-default-mode))
           ((sumibi-modeline-message-mode-p)         (sumibi-modeline-message-mode))
           ((sumibi-modeline-elfeed-search-mode-p)   (sumibi-modeline-elfeed-search-mode))
           ((sumibi-modeline-elfeed-show-mode-p)     (sumibi-modeline-elfeed-show-mode))
           ((sumibi-modeline-deft-mode-p)            (sumibi-modeline-deft-mode))
           ((sumibi-modeline-info-mode-p)            (sumibi-modeline-info-mode))
           ((sumibi-modeline-calendar-mode-p)        (sumibi-modeline-calendar-mode))
           ((sumibi-modeline-org-capture-mode-p)     (sumibi-modeline-org-capture-mode))
           ((sumibi-modeline-org-agenda-mode-p)      (sumibi-modeline-org-agenda-mode))
           ((sumibi-modeline-org-clock-mode-p)       (sumibi-modeline-org-clock-mode))
           ((sumibi-modeline-term-mode-p)            (sumibi-modeline-term-mode))
           ((sumibi-modeline-vterm-mode-p)           (sumibi-modeline-term-mode))
           ((sumibi-modeline-mu4e-dashboard-mode-p)  (sumibi-modeline-mu4e-dashboard-mode))
           ((sumibi-modeline-mu4e-main-mode-p)       (sumibi-modeline-mu4e-main-mode))
           ((sumibi-modeline-mu4e-headers-mode-p)    (sumibi-modeline-mu4e-headers-mode))
;;         ((sumibi-modeline-mu4e-view-mode-p)       (sumibi-modeline-mu4e-view-mode))
           ((sumibi-modeline-text-mode-p)            (sumibi-modeline-default-mode))
           ((sumibi-modeline-pdf-view-mode-p)        (sumibi-modeline-pdf-view-mode))
	   ((sumibi-modeline-docview-mode-p)         (sumibi-modeline-docview-mode))
	   ((sumibi-modeline-completion-list-mode-p) (sumibi-modeline-completion-list-mode))
           ((sumibi-modeline-sumibi-help-mode-p)       (sumibi-modeline-sumibi-help-mode))
           (t                                      (sumibi-modeline-default-mode)))))))

;; ---------------------------------------------------------------------
(defun sumibi-modeline-update-windows ()
  "Modify the mode line depending on the presence of a window
below or a buffer local variable 'no-mode-line'."
  (dolist (window (window-list))
    (with-selected-window window
	  (with-current-buffer (window-buffer window)
        (if (or (not (boundp 'no-mode-line)) (not no-mode-line))
            (setq mode-line-format 
                  (cond ((one-window-p t) (list ""))
                        ((eq (window-in-direction 'below) (minibuffer-window)) (list ""))
                        ((not (window-in-direction 'below)) (list ""))
                        (t nil))))))))

(add-hook 'window-configuration-change-hook 'sumibi-modeline-update-windows)

(setq eshell-status-in-modeline nil)
;; (setq-default mode-line-format (list "%-"))
(setq-default mode-line-format "")
(sumibi-modeline)

(provide 'sumibi-modeline)



