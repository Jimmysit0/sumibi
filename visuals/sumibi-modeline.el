;; sumibi mode line format:
;;
;; [ status | name | primary                               secondary ]
;;
;; -------------------------------------------------------------------
(require 'subr-x)
;; (require 'all-the-icons)

;; ---------------------------------------------------------------------
(defun vc-branch ()
  (if vc-mode
	  (let ((backend (vc-backend buffer-file-name)))
		(concat "#" (substring-no-properties vc-mode
								 (+ (if (eq backend 'Hg) 2 3) 2))))  nil))


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

;; ---------------------------------------------------------------------
(defun sumibi-modeline-compose (status name primary secondary &optional pad)
  "Compose a string with provided information"
  (let* ((status-face-rw `(:foreground ,(face-background 'sumibi-face-default)
						   :background ,(face-foreground 'sumibi-face-faded)))
		 (status-face-ro `(:foreground ,(face-background 'sumibi-face-default)
						   :background ,(face-foreground 'sumibi-face-popout)))
		 (status-face-** `(:foreground ,(face-background 'sumibi-face-default)
						   :background ,(face-background 'sumibi-face-critical)))
		 (pad            (or pad 1))
		 (space-up       +0.15)
		 (space-down     -0.20)
		 (left (concat
				(cond ((string= status "RO")
					   (propertize " RO " 'face status-face-ro))
					  ((string= status "**")
					   (propertize " ** " 'face status-face-**))
					  ((string= status "RW")
					   (propertize " RW " 'face status-face-rw))
					  (t (propertize status 'face status-face-ro))
					  )
				(propertize " " 'display `(raise ,space-up))
				(propertize name 'face 'sumibi-face-strong)
				(propertize " " 'display `(raise ,space-down)) primary))
		 (right secondary)
;;         (available-width (- (window-total-width nil 'floor) (length left) 1)))
		 (available-width (- (window-body-width) (length left) pad)))
	(format (format "%%s%%%ds" available-width) left right)))

;; ---------------------------------------------------------------------
(defun sumibi-modeline-mu4e-dashboard-mode-p ()
  (bound-and-true-p mu4e-dashboard-mode))

(defun sumibi-modeline-mu4e-dashboard-mode ()
  (sumibi-modeline-compose (sumibi-modeline-status)
			 ;; (sumibi-modeline-status-icon "envelope")
						 "Mail"
						 (sumibi-modeline-mu4e-context)
						 ""))

;; ---------------------------------------------------------------------
(defun sumibi-modeline-elfeed-search-mode-p ()
  (derived-mode-p 'elfeed-search-mode))

(defun sumibi-modeline-elfeed-search-mode ()
  (sumibi-modeline-compose (sumibi-modeline-status)
						 ;; (sumibi-modeline-status-icon "newspaper-o")
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
	(sumibi-modeline-compose status
			   ;; (sumibi-modeline-status-icon "newspaper-o")
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
						 "[C-c C-c] Finish [C-c C-w] Refile [C-c C-k] Abort"))

(with-eval-after-load 'org-capture
  (defun org-capture-turn-off-header-line ()
	(setq-local header-line-format (default-value 'header-line-format))
	;; (fit-window-to-buffer nil nil 8)
	;; (face-remap-add-relative 'header-line '(:background "#ffffff"))
	(message nil))
  (add-hook 'org-capture-mode-hook
			#'org-capture-turn-off-header-line))

;; ---------------------------------------------------------------------
(defun sumibi-modeline-org-agenda-mode-p ()
  (derived-mode-p 'org-agenda-mode))

(defun sumibi-modeline-org-agenda-mode ()
  (sumibi-modeline-compose (sumibi-modeline-status)
			 ;; (sumibi-modeline-status-icon "calendar")
						 "Agenda"
						 ""
						 (format-time-string "%H:%M")
						 ))

;; ---------------------------------------------------------------------
(defun sumibi-modeline-term-mode-p ()
  (derived-mode-p 'term-mode))

(defun sumibi-modeline-term-mode ()
  (sumibi-modeline-compose " >_ "
			 ;; (sumibi-modeline-status-icon "terminal")
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
			 ;;(sumibi-modeline-status-icon "envelope-o")
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

(add-hook 'mu4e-view-mode-hook
		  (lambda () (setq header-line-format "%-")
					 (face-remap-add-relative 'header-line
											  '(:background "#ffffff" :height 1.0))))

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
		  (mode-name   (format-mode-line "%m"))
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
(defun sumibi-modeline-default-mode ()
	(let ((buffer-name (format-mode-line "%b"))
		  (mode-name   (format-mode-line "%m"))
		  (branch      (vc-branch))
		  (position    (format-mode-line "%l:%c")))
	  (sumibi-modeline-compose (sumibi-modeline-status)
							 buffer-name
							 (concat "(" mode-name
									 (if branch (concat ", "
											(propertize branch 'face 'italic)))
									 ")" )
							 position)))

;; ---------------------------------------------------------------------
(defun sumibi-modeline-status-icon (name &optional fmt fg-color bg-color)
  "Return a propertized string containing icon NAME from the font
   awesome family using the provided FMT to format the string (%s
   is replaced with icon, default is ' %s  '). Foreground (FG-COLOR)
   and background (BG-COLOR) can be specified. If not, foreground
   color is the default background (white) and background is the
   foreground of the 'sumibi-face-popout face."

  (let ((fg-color (or fg-color (face-background 'sumibi-face-default)))
		(bg-color (or bg-color (face-foreground 'sumibi-face-popout)))
		(fmt      (or fmt "  %s  ")))

	(propertize (format fmt (all-the-icons-faicon name))
				'face `(:family ,(all-the-icons-faicon-family)
								:height 1.1
								:foreground ,fg-color
								:background ,bg-color)
				'display '(raise 0.0))))

;; ---------------------------------------------------------------------
(defun sumibi-modeline-status ()
  "Return buffer status: read-only (RO), modified (**) or read-write (RW)"

  (let ((read-only   buffer-read-only)
		(modified    (and buffer-file-name (buffer-modified-p))))
	(cond (modified  "**") (read-only "RO") (t "RW"))))

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
	 (cond ((sumibi-modeline-elfeed-search-mode-p)   (sumibi-modeline-elfeed-search-mode))
		   ((sumibi-modeline-elfeed-show-mode-p)     (sumibi-modeline-elfeed-show-mode))
		   ((sumibi-modeline-calendar-mode-p)        (sumibi-modeline-calendar-mode))
		   ((sumibi-modeline-org-capture-mode-p)     (sumibi-modeline-org-capture-mode))
		   ((sumibi-modeline-org-agenda-mode-p)      (sumibi-modeline-org-agenda-mode))
		   ((sumibi-modeline-org-clock-mode-p)       (sumibi-modeline-org-clock-mode))
		   ((sumibi-modeline-term-mode-p)            (sumibi-modeline-term-mode))
		   ((sumibi-modeline-mu4e-dashboard-mode-p)  (sumibi-modeline-mu4e-dashboard-mode))
		   ((sumibi-modeline-mu4e-main-mode-p)       (sumibi-modeline-mu4e-main-mode))
		   ((sumibi-modeline-mu4e-headers-mode-p)    (sumibi-modeline-mu4e-headers-mode))
;;           ((sumibi-modeline-mu4e-view-mode-p)       (sumibi-modeline-mu4e-view-mode))
		   (t                                      (sumibi-modeline-default-mode)))))))

;; ---------------------------------------------------------------------
(defun sumibi-modeline-update-windows ()
  "Modify the mode line depending on the presence of a window below."

  (dolist (window (window-list))
	(with-selected-window window
	  (if (or (one-window-p t)
		  (eq (window-in-direction 'below) (minibuffer-window))
		  (not (window-in-direction 'below)))
		  (with-current-buffer (window-buffer window)
			(setq mode-line-format "%-"))
		(with-current-buffer (window-buffer window)
		  (setq mode-line-format nil)))
;;      (if (window-in-direction 'above)
;;		  (face-remap-add-relative 'header-line '(:overline "#777777"))
;;		(face-remap-add-relative 'header-line '(:overline nil)))
	  )))
(add-hook 'window-configuration-change-hook 'sumibi-modeline-update-windows)

(setq-default mode-line-format "%-")
(sumibi-modeline)

(provide 'sumibi-modeline)
