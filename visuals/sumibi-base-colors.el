;;; base-colors -*- lexical-binding: t; -*-

(defgroup sumibi '()
  "Faces and colors for the sumibi emacs theme")

;; Derive our default color set from classic Emacs faces.
;; This allows dropping sumibi components into already themed Emacsen with varying
;; degrees of visual appeal.
;;
;; We memorize the default colorset in this var in order not to confuse
;; customize: the STANDARD argument of defcustom gets re-evaluated by customize
;; to determine if the current value is default or not.
(defvar sumibi-base-colors--defaults
  `((foreground . ,(face-foreground 'default nil t))
    (background . ,(face-background 'default nil t))
    (highlight . ,(face-background 'fringe nil t))
    (critical . ,(face-foreground 'error nil t))
    (salient . ,(face-foreground 'font-lock-keyword-face nil t))
    (strong . ,(face-foreground 'default nil t))
    (popout . ,(face-foreground 'font-lock-string-face nil t))
    (subtle . ,(face-background 'mode-line-inactive nil t))
    (faded . ,(face-foreground 'shadow nil t))))

(defun sumibi-base-colors--get (name)
  "Get default color associated with symbol NAME."
  (cdr (assoc name sumibi-base-colors--defaults)))

(defcustom sumibi-color-foreground (sumibi-base-colors--get 'foreground)
  ""
  :type 'color
  :group 'sumibi)

(defcustom sumibi-color-background (sumibi-base-colors--get 'background)
  ""
  :type 'color
  :group 'sumibi)

(defcustom sumibi-color-highlight (sumibi-base-colors--get 'highlight)
  ""
  :type 'color
  :group 'sumibi)

(defcustom sumibi-color-critical (sumibi-base-colors--get 'critical)
  ""
  :type 'color
  :group 'sumibi)

(defcustom sumibi-color-salient (sumibi-base-colors--get 'salient)
  ""
  :type 'color
  :group 'sumibi)

(defcustom sumibi-color-strong (sumibi-base-colors--get 'strong)
  ""
  :type 'color
  :group 'sumibi)

(defcustom sumibi-color-popout (sumibi-base-colors--get 'popout)
  ""
  :type 'color
  :group 'sumibi)

(defcustom sumibi-color-subtle (sumibi-base-colors--get 'subtle)
  ""
  :type 'color
  :group 'sumibi)

(defcustom sumibi-color-faded (sumibi-base-colors--get 'faded)
  ""
  :type 'color
  :group 'sumibi)

(provide 'sumibi-base-colors)
