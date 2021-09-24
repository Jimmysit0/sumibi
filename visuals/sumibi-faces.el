;;; sumibi-faces --- Face settings for sumibi-emacs  -*- lexical-binding: t; -*-

(require 'sumibi-base-colors)

(defcustom sumibi-font-family-monospaced "Roboto Mono"
  "Name of the font-family to use for sumibi.
Defaults to Roboto Mono. Customizing this might lead to conflicts
if the family does not have sufficient bold/light etc faces."
  :group 'sumibi
  :type 'string)

(defcustom sumibi-font-family-proportional nil
  "Font to use for variable pitch faces.
Setting this allows sumibi to display variable pitch faces when,
for instance, 'variable-pitch-mode' or 'mixed-pitch-mode' is active in a buffer.
Defaults to nil."
  :group 'sumibi
  :type 'string)

(defcustom sumibi-font-size 14
  "Default value for the font size of sumibi-theme in pt units.
Note: to change this after startup, call
\(sumibi-faces\) and \(sumibi-themes\)."
  :group 'sumibi
  :type 'integer)

;; A theme is fully defined by these seven faces

(defface sumibi-face-default nil
  "Default face is used for regular information."
  :group 'sumibi)

(defface sumibi-face-variable-pitch nil
  "Default variable-pitch face is used for variable pitch mode."
  :group 'sumibi)

(defface sumibi-face-critical nil
  "Critical face is for information that requires immediate action.
It should be of high constrast when compared to other faces. This
can be realized (for example) by setting an intense background
color, typically a shade of red. It must be used scarcely."
  :group 'sumibi)

(defface sumibi-face-popout nil
  "Popout face is used for information that needs attention.
To achieve such effect, the hue of the face has to be
sufficiently different from other faces such that it attracts
attention through the popout effect."
  :group 'sumibi)

(defface sumibi-face-strong nil
  "Strong face is used for information of a structural nature.
It has to be the same color as the default color and only the
weight differs by one level (e.g., light/regular or
regular/bold). IT is generally used for titles, keywords,
directory, etc."
  :group 'sumibi)

(defface sumibi-face-salient nil
  "Salient face is used for information that are important.
To suggest the information is of the same nature but important,
the face uses a different hue with approximately the same
intensity as the default face. This is typically used for links."
  :group 'sumibi)

(defface sumibi-face-faded nil
  "Faded face is for information that are less important.
It is made by using the same hue as the default but with a lesser
intensity than the default. It can be used for comments,
secondary information and also replace italic (which is generally
abused anyway)."
  :group 'sumibi)

(defface sumibi-face-subtle nil
  "Subtle face is used to suggest a physical area on the screen.
It is important to not disturb too strongly the reading of
information and this can be made by setting a very light
background color that is barely perceptible."
  :group 'sumibi)

(defface sumibi-face-header-default nil
  "Default face for ther header line."
  :group 'sumibi)

(defface sumibi-face-header-critical nil
  "Critical face for ther header line."
  :group 'sumibi)

(defface sumibi-face-header-popout nil
  "Popout face for ther header line."
  :group 'sumibi)

(defface sumibi-face-header-strong nil
  "Strong face for ther header line."
  :group 'sumibi)

(defface sumibi-face-header-salient nil
  "Salient face for ther header line."
  :group 'sumibi)

(defface sumibi-face-header-faded nil
  "Faded face for ther header line."
  :group 'sumibi)

(defface sumibi-face-header-subtle nil
  "Subtle face for ther header line."
  :group 'sumibi)

(defface sumibi-face-header-highlight nil
  "Highlight face for ther header line."
  :group 'sumibi)

(defface sumibi-face-header-separator nil
  "Face for separating item in the header line (internal use)"
  :group 'sumibi)

(defface sumibi-face-header-filler nil
  "Face compsenting spaces in the header line (internal use) "
  :group 'sumibi)

(defface sumibi-face-tag-default nil
  "Default face for tags"
  :group 'sumibi)

(defface sumibi-face-tag-faded nil
  "Faded face for tags"
  :group 'sumibi)

(defface sumibi-face-tag-strong nil
  "Strong face for tags"
  :group 'sumibi)

(defface sumibi-face-tag-salient nil
  "Salient face for tags"
  :group 'sumibi)

(defface sumibi-face-tag-popout nil
  "Popout face for tags"
  :group 'sumibi)

(defface sumibi-face-tag-critical nil
  "Critical face for tags"
  :group 'sumibi)

(defun sumibi-what-faces (pos)
  "Get the font faces at POS."
  (interactive "d")
  (let ((faces (remq nil
                     (list
                      (get-char-property pos 'read-face-name)
                      (get-char-property pos 'face)
                      (plist-get (text-properties-at pos) 'face)))))
    (message "Faces: %s" faces)))

(defun sumibi-faces ()
  "Derive face attributes for sumibi-faces using sumibi-theme values."
  (set-face-attribute 'sumibi-face-default nil
                      :foreground sumibi-color-foreground
                      :background sumibi-color-background
                      :family     sumibi-font-family-monospaced
                      :height       (* sumibi-font-size 10))
  (set-face-attribute 'sumibi-face-critical nil
                      :foreground sumibi-color-foreground
                      :background sumibi-color-critical)
  (set-face-attribute 'sumibi-face-popout nil
                      :foreground sumibi-color-popout)

  (set-face-attribute 'sumibi-face-variable-pitch nil
                          :foreground (face-foreground 'sumibi-face-default)
                          :background (face-background 'sumibi-face-default)
                          :family sumibi-font-family-proportional
                          :height (* sumibi-font-size 10))
  (if (display-graphic-p)
      (set-face-attribute 'sumibi-face-strong nil
                          :foreground (face-foreground 'sumibi-face-default)
                          :weight 'bold)
    (set-face-attribute 'sumibi-face-strong nil
                        :foreground (face-foreground 'sumibi-face-default)
                        :weight 'bold))

  (set-face-attribute 'sumibi-face-salient nil
                      :foreground sumibi-color-salient
                      :weight 'light)

  (set-face-attribute 'sumibi-face-faded nil
                      :foreground sumibi-color-faded
                      :weight 'light)

  (set-face-attribute 'sumibi-face-subtle nil
                      :background sumibi-color-subtle)

  (set-face-attribute 'sumibi-face-header-default nil
                      :foreground sumibi-color-foreground
                      :background sumibi-color-subtle
                      :box `(:line-width 1
                                         :color ,sumibi-color-background
                                         :style nil))

  (set-face-attribute 'sumibi-face-tag-default nil
                      :foreground sumibi-color-foreground
                      :background sumibi-color-background
                      :weight 'regular
                      :height (if (display-graphic-p)
                                  (round
                                   (* 0.85 (* 10 sumibi-font-size)))
                                                1)
                      :box `(:line-width 1
                                         :color ,sumibi-color-foreground
                                         :style nil))

  (set-face-attribute 'sumibi-face-header-strong nil
                      :foreground sumibi-color-strong
                      :background sumibi-color-subtle
                      :inherit 'sumibi-face-strong
                      :box `(:line-width 1
                                         :color ,sumibi-color-background
                                         :style nil))

  (set-face-attribute 'sumibi-face-tag-strong nil
                      :foreground sumibi-color-strong
                      :background sumibi-color-subtle
                      :weight 'regular
                      :height (if (display-graphic-p)
                                  (round
                                   (* 0.85 (* 10 sumibi-font-size)))
                                                1)
                      :box `(:line-width 1
                                         :color ,sumibi-color-strong
                                         :style nil))

  (set-face-attribute 'sumibi-face-header-salient nil
                      :foreground sumibi-color-background
                      :background sumibi-color-salient
                      :box `(:line-width 1
                                         :color ,sumibi-color-background
                                         :style nil))

  (set-face-attribute 'sumibi-face-tag-salient nil
                      :foreground sumibi-color-background
                      :background sumibi-color-salient
                      :weight 'regular
                      :height (if (display-graphic-p)
                                  (round
                                   (* 0.85 (* 10 sumibi-font-size)))
                                                1)
                      :box `(:line-width 1
                                         :color ,sumibi-color-salient
                                         :style nil))

  (set-face-attribute 'sumibi-face-header-popout nil
                      :foreground sumibi-color-background
                      :background sumibi-color-popout
                      :box `(:line-width 1
                                         :color ,sumibi-color-background
                                         :style nil))

  (set-face-attribute 'sumibi-face-tag-popout nil
                      :foreground sumibi-color-background
                      :background sumibi-color-popout
                      :weight 'regular
                      :height (if (display-graphic-p)
                                  (round
                                   (* 0.85 (* 10 sumibi-font-size)))
                                                1)
                      :box `(:line-width 1
                                         :color ,sumibi-color-popout
                                         :style nil))

  (set-face-attribute 'sumibi-face-header-faded nil
                      :foreground sumibi-color-background
                      :background sumibi-color-faded
                      :box `(:line-width 1
                                         :color ,sumibi-color-background
                                         :style nil))

  (set-face-attribute 'sumibi-face-tag-faded nil
                      :foreground sumibi-color-background
                      :background sumibi-color-faded
                      :weight 'regular
                      :height (if (display-graphic-p)
                                  (round
                                   (* 0.85 (* 10 sumibi-font-size)))
                                                1)
                      :box `(:line-width 1
                                         :color ,sumibi-color-faded
                                         :style nil))

  (set-face-attribute 'sumibi-face-header-subtle nil)

  (set-face-attribute 'sumibi-face-header-critical nil
                      :foreground sumibi-color-background
                      :background sumibi-color-critical
                      :box `(:line-width 1
                                         :color ,sumibi-color-background
                                         :style nil))
  (set-face-attribute 'sumibi-face-tag-critical nil
                      :foreground sumibi-color-background
                      :background sumibi-color-critical
                      :weight 'regular
                      :height (if (display-graphic-p)
                                  (round
                                   (* 0.85 (* 10 sumibi-font-size)))
                                                1)
                      :box `(:line-width 1
                                         :color ,sumibi-color-critical
                                         :style nil))

  (set-face-attribute 'sumibi-face-header-separator nil
                      :inherit 'sumibi-face-default
                      :height 0.1)
  (set-face-attribute 'sumibi-face-header-filler nil
                      :inherit 'sumibi-face-header-default
                      :height 0.1)
  (set-face-attribute 'sumibi-face-header-highlight nil
                      :inherit 'sumibi-face-header-faded
                      :box nil))

 (defun sumibi-theme--flyspell ()
  "Derive flyspell faces from sumibi faces."
  (with-eval-after-load 'flyspell
    (set-face 'flyspell-duplicate                     'sumibi-face-default)
    (set-face-attribute 'flyspell-incorrect nil
                  :inherit 'sumibi-face-critical)))

(defun sumibi-theme--flycheck ()
  "Derive flycheck faces from sumibi faces."
  (with-eval-after-load 'flycheck
    (set-face-attribute 'flycheck-error nil
                        :underline `(:style wave :color ,sumibi-color-critical)    :background sumibi-color-background)
    (set-face-attribute 'flycheck-warning nil
                        :underline `(:style wave :color ,sumibi-color-critical)
                        :background sumibi-color-background)
    (set-face-attribute 'flycheck-info  nil
                        :underline `(:style wave :color ,sumibi-color-faded)
                        :background sumibi-color-background)))

(defun sumibi-theme--company ()
  "Derive company faces from sumibi faces."
  (with-eval-after-load 'company
    (set-face 'company-echo-common              'sumibi-face-faded)
    (set-face 'company-preview-common           'sumibi-face-default)
    (set-face 'company-preview-search           'sumibi-face-faded)
    (set-face 'company-scrollbar-bg             'sumibi-face-default)
    (set-face 'company-scrollbar-fg             'sumibi-face-default)
    (set-face 'company-tooltip-annotation       'sumibi-face-faded)
    (set-face 'company-tooltip-common           'sumibi-face-faded)
    (set-face-attribute 'company-tooltip-common-selection nil
                        :background sumibi-color-background
                        :foreground sumibi-color-critical :weight 'bold)
    (set-face-attribute 'company-tooltip-selection nil
                        :background sumibi-color-background
                        :foreground sumibi-color-critical)
    (set-face-attribute 'company-preview nil
                        :background sumibi-color-background
                        :foreground sumibi-color-foreground)
    (set-face-attribute 'company-tooltip nil
                        :background sumibi-color-background
                        :foreground sumibi-color-foreground)))

(defun sumibi-theme--ctrlf ()
  "Derive ctrlf faces from sumibi faces."
  (with-eval-after-load 'ctrlf
    (set-face 'ctrlf-highlight-active 'sumibi-face-strong)
    (set-face 'ctrlf-highlight-line   'sumibi-face-highlight)))

(defun sumibi-theme--rainbow-delimiters ()
  "Derive rainbow-delimiters faces from sumibi faces."
  (with-eval-after-load 'rainbow-delimiters
    (set-face-attribute 'rainbow-delimiters-depth-1-face nil
                        :foreground sumibi-color-critical)
    (set-face-attribute 'rainbow-delimiters-depth-2-face nil
                        :foreground sumibi-color-foreground)
    (set-face-attribute 'rainbow-delimiters-depth-3-face nil
                        :foreground sumibi-color-foreground)
    (set-face-attribute 'rainbow-delimiters-depth-4-face nil
                        :foreground sumibi-color-foreground)
    (set-face-attribute 'rainbow-delimiters-depth-5-face nil
                        :foreground sumibi-color-foreground)
    (set-face-attribute 'rainbow-delimiters-depth-6-face nil
                        :foreground sumibi-color-foreground)
    (set-face-attribute 'rainbow-delimiters-depth-7-face nil
                        :foreground sumibi-color-foreground)
    (set-face-attribute 'rainbow-delimiters-depth-8-face nil
                        :foreground sumibi-color-foreground)
    (set-face-attribute 'rainbow-delimiters-depth-9-face nil
                        :foreground sumibi-color-foreground)
    (set-face-attribute 'rainbow-delimiters-base-error-face nil
                        :foreground sumibi-color-critical
                        :background sumibi-color-background :weight 'bold)))


(provide 'sumibi-faces)
;;; sumibi-faces.el ends here
