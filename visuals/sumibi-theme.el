;;; -*- lexical-binding: t; -*-

(require 'sumibi-faces)

;; When we set a face, we take care of removing any previous settings
(defun set-face (face style)
  "Reset FACE and make it inherit STYLE."
  (set-face-attribute face nil
                      :foreground 'unspecified :background 'unspecified
                      :family     'unspecified :slant      'unspecified
                      :weight     'unspecified :height     'unspecified
                      :underline  'unspecified :overline   'unspecified
                      :box        'unspecified :inherit    style))

(defun sumibi-theme--basics ()
  "Derive basic Emacs faces from sumibi-faces and sumibi-color-theme."
  (set-foreground-color sumibi-color-foreground)
  (set-background-color sumibi-color-background)

  ;; THIS
  (set-face-attribute 'default nil
                      :foreground (face-foreground 'default)
                      :background (face-background 'default)
                      :weight     'regular
                      :family     (face-attribute 'sumibi-face-default :family)
                      :height     (face-attribute 'sumibi-face-default :height))
  (if (display-graphic-p)
      (set-face-attribute 'bold nil :weight 'regular)
    (set-face-attribute 'bold nil :weight 'bold))

  ;; Structural
  (set-face 'bold                                     'sumibi-face-strong)
  (set-face 'italic                                    'sumibi-face-faded)
  (set-face 'bold-italic                              'sumibi-face-strong)
  (set-face-attribute 'region nil
                      :background "#ffd6e8"
                      :foreground sumibi-color-foreground)
  (set-face 'highlight                                'sumibi-face-subtle)
  (set-face 'fixed-pitch-serif                       'sumibi-face-default)
  (set-face 'cursor                                  'sumibi-face-default)
  (if 'sumibi-font-family-proportional
      (set-face-attribute 'variable-pitch nil ;; to work with mixed-pitch
                :foreground (face-foreground 'default)
                :background (face-background 'default)
                :family     (face-attribute 'sumibi-face-variable-pitch :family)
                :height     (face-attribute 'sumibi-face-variable-pitch :height)
                :weight     'light)
      (set-face 'variable-pitch                     'sumibi-face-default))

  (set-face-attribute 'cursor nil
                      :background sumibi-color-salient)
  (set-face-attribute 'window-divider nil
                      :foreground (face-background 'sumibi-face-default))
  (set-face-attribute 'window-divider-first-pixel nil
                      :foreground sumibi-color-background)
  (set-face-attribute 'window-divider-last-pixel nil
                      :foreground sumibi-color-background)
  (set-face-foreground 'vertical-border sumibi-color-subtle)

  ;; Semantic
  (set-face 'shadow                                    'sumibi-face-faded)
  (set-face 'success                                 'sumibi-face-salient)
  (set-face 'warning                                  'sumibi-face-popout)
  (set-face 'error                                  'sumibi-face-critical)
  (set-face 'match                                    'sumibi-face-popout)
  
  ;; General
  (set-face 'buffer-menu-buffer                       'sumibi-face-strong)
  (set-face 'minibuffer-prompt                        'sumibi-face-strong)
  (set-face 'link                                    'sumibi-face-salient)
  (set-face 'fringe                                    'sumibi-face-faded)
  (set-face-attribute 'fringe nil
                      :foreground (face-background 'sumibi-face-subtle)
                      :background (face-background 'default))
  (set-face 'isearch                                  'sumibi-face-strong)
  (set-face 'isearch-fail                              'sumibi-face-faded)
  (set-face 'lazy-highlight                           'sumibi-face-subtle)
  (set-face 'trailing-whitespace                      'sumibi-face-subtle)
  (set-face-attribute 'show-paren-match nil
                      :foreground sumibi-color-salient
                      :background sumibi-color-background
                      :underline t)
  (set-face 'show-paren-mismatch                           'face-normal)
  (set-face-attribute 'tooltip nil                         :height 0.85)
  (set-face 'secondary-selection                      'sumibi-face-subtle)
  (set-face 'completions-common-part                   'sumibi-face-faded)
  (set-face 'completions-first-difference            'sumibi-face-default))

(defun sumibi-theme--font-lock ()
  "Derive font-lock faces from sumibi-faces."
  (set-face-attribute 'font-lock-comment-face nil
                      :background "#fdf0ca"
                      :foreground sumibi-color-foreground)
  (set-face-attribute 'font-lock-doc-face nil
                      :background "#fdf0ca"
                      :foreground sumibi-color-foreground
                      :bold 'nil)
  (set-face-attribute 'font-lock-string-face nil
                      :background "#e0e0e0"
                      :foreground sumibi-color-foreground)
  (set-face-attribute 'font-lock-constant-face nil
                      :background "#e0e0e0"
                      :foreground sumibi-color-foreground)
  (set-face-attribute 'font-lock-warning-face nil
                      :background sumibi-color-background
                      :foreground sumibi-color-critical)
  (set-face-attribute 'font-lock-function-name-face nil
                      :background "#d0e2ff"
                      :foreground sumibi-color-foreground)
  (set-face-attribute 'font-lock-variable-name-face nil
                      :background "#e0e0e0"
                      :foreground sumibi-color-foreground)
  (set-face-attribute 'font-lock-builtin-face nil
                      :background "#e0e0e0"
                      :foreground sumibi-color-foreground)
  (set-face-attribute 'font-lock-type-face nil
                      :background "#e0e0e0"
                      :foreground sumibi-color-foreground)
  (set-face-attribute 'font-lock-keyword-face nil
                      :background "#e0e0e0"
                      :foreground sumibi-color-foreground))

(defun sumibi-theme--mode-line ()
  "Derive mode-line and header-line faces from sumibi-faces."
  (set-face-attribute 'mode-line nil
                      :foreground sumibi-color-faded
                      :background sumibi-color-background
                      :weight 'bold
                      :box `(:line-width 1
                             :color ,(face-background 'sumibi-face-default)
                             :style nil))
  (set-face-attribute 'mode-line-inactive nil
                      :foreground sumibi-color-strong
                      :background sumibi-color-background
                      :box `(:line-width 1
                      :color ,(face-background 'sumibi-face-default)
                      :style nil))
  (set-face-attribute 'mode-line-buffer-id nil
                      :foreground sumibi-color-strong
                      :bold t)
  (set-face-attribute 'mode-line-emphasis nil
                      :foreground sumibi-color-strong
                      :bold t)
  (set-face-attribute 'mode-line-highlight nil
                      :foreground sumibi-color-strong)
  (set-face-attribute 'header-line nil
                      :foreground (face-foreground 'sumibi-face-faded)
                      :background (face-background 'sumibi-face-default)
                      :weight 'bold
                      :overline nil
                      :underline nil
                      :box `(:line-width 1
                                         :color ,(face-background 'sumibi-face-default)
                                         :style nil)
                      :inherit nil)
  (set-face-attribute 'internal-border nil
                       :background (face-background 'sumibi-face-default)))


(defun sumibi-theme--minibuffer ()
  "Derive minibuffer / echo area faces from sumibi faces."
  ;; Minibuffer / echo area
  (dolist (buffer (list " *Minibuf-0*" " *Echo Area 0*"
                        " *Minibuf-1*" " *Echo Area 1*"))
    (when (get-buffer buffer)
      (with-current-buffer buffer
        (face-remap-add-relative 'default 'sumibi-face-faded)))))


(defun sumibi-theme--hl-line ()
  "Derive hl-line faces from sumibi faces."
  (with-eval-after-load 'hl-line
    (set-face-attribute 'hl-line nil
                        :background sumibi-color-highlight)))

(defun sumibi-theme--buttons ()
  "Derive button faces from sumibi faces."
  ;; Buttons
  (with-eval-after-load 'cus-edit
    (set-face-attribute 'custom-button nil
                         :foreground (face-foreground 'sumibi-face-faded)
                         :background (face-background 'sumibi-face-default)
                         :box `(:line-width 1
                                :color ,(face-foreground 'sumibi-face-faded)
                                :style nil))
    (set-face-attribute 'custom-button-mouse nil
                         ;;                      :inherit 'custom-button
                         :foreground (face-foreground 'sumibi-face-faded)
                         :background (face-background 'sumibi-face-subtle)
                         :box `(:line-width 1
                                            :color ,(face-foreground 'sumibi-face-faded)
                                            :style nil))
    (set-face-attribute 'custom-button-pressed nil
                         :foreground (face-background 'default)
                         :background (face-foreground 'sumibi-face-salient)
                         :inherit 'sumibi-face-salient
                         :box `(:line-width 1
                                            :color ,(face-foreground 'sumibi-face-salient)
                                            :style nil)
                         :inverse-video nil)))

(defun sumibi-theme--info ()
  "Derive info faces from sumibi faces."
  (with-eval-after-load 'info
    (set-face 'info-menu-header                       'sumibi-face-strong)
    (set-face 'info-header-node                      'sumibi-face-default)
    (set-face 'info-index-match                      'sumibi-face-salient)
    (set-face 'Info-quoted                             'sumibi-face-faded)
    (set-face 'info-title-1                           'sumibi-face-strong)
    (set-face 'info-title-2                           'sumibi-face-strong)
    (set-face 'info-title-3                           'sumibi-face-strong)
    (set-face 'info-title-4                           'sumibi-face-strong)))


(defun sumibi-theme--speedbar ()
  "Derive speedbar faces from sumibi faces "
  (with-eval-after-load 'speedbar
    (set-face 'speedbar-button-face                    'sumibi-face-faded)
    (set-face 'speedbar-directory-face                'sumibi-face-strong)
    (set-face 'speedbar-file-face                    'sumibi-face-default)
    (set-face 'speedbar-highlight-face             'sumibi-face-highlight)
    (set-face 'speedbar-selected-face                 'sumibi-face-subtle)
    (set-face 'speedbar-separator-face                 'sumibi-face-faded)
    (set-face 'speedbar-tag-face                       'sumibi-face-faded)))


(defun sumibi-theme--bookmark ()
  "Derive bookmark faces from sumibi faces."
  (with-eval-after-load 'bookmark
    (set-face 'bookmark-menu-heading                  'sumibi-face-strong)
    (set-face 'bookmark-menu-bookmark                'sumibi-face-salient)))


(defun sumibi-theme--message ()
  "Derive message faces from sumibi faces."
  (with-eval-after-load 'message
    (unless (version< emacs-version "27.0")
      (set-face 'message-cited-text-1                  'sumibi-face-faded)
      (set-face 'message-cited-text-2                  'sumibi-face-faded)
      (set-face 'message-cited-text-3                  'sumibi-face-faded)
      (set-face 'message-cited-text-4                 'sumibi-face-faded))
    (set-face 'message-cited-text                      'sumibi-face-faded)
    (set-face 'message-header-cc                     'sumibi-face-default)
    (set-face 'message-header-name                    'sumibi-face-strong)
    (set-face 'message-header-newsgroups             'sumibi-face-default)
    (set-face 'message-header-other                  'sumibi-face-default)
    (set-face 'message-header-subject                'sumibi-face-salient)
    (set-face 'message-header-to                     'sumibi-face-salient)
    (set-face 'message-header-xheader                'sumibi-face-default)
    (set-face 'message-mml                            'sumibi-face-popout)
    (set-face 'message-separator                       'sumibi-face-faded)))


(defun sumibi-theme--outline ()
  "Derive outline faces from sumibi faces."
  (with-eval-after-load 'outline
    (set-face 'outline-1                              'sumibi-face-strong)
    (set-face 'outline-2                              'sumibi-face-strong)
    (set-face 'outline-3                              'sumibi-face-strong)
    (set-face 'outline-4                              'sumibi-face-strong)
    (set-face 'outline-5                              'sumibi-face-strong)
    (set-face 'outline-6                              'sumibi-face-strong)
    (set-face 'outline-7                              'sumibi-face-strong)
    (set-face 'outline-8                              'sumibi-face-strong)))


(defun sumibi-theme--customize ()
  "Derive customize faces from sumibi faces."
  (with-eval-after-load 'cus-edit
    (set-face 'widget-field                           'sumibi-face-subtle)
    (set-face 'widget-button                          'sumibi-face-strong)
    (set-face 'widget-single-line-field               'sumibi-face-subtle)
    (set-face 'custom-group-subtitle                  'sumibi-face-strong)
    (set-face 'custom-group-tag                       'sumibi-face-strong)
    (set-face 'custom-group-tag-1                     'sumibi-face-strong)
    (set-face 'custom-comment                          'sumibi-face-faded)
    (set-face 'custom-comment-tag                      'sumibi-face-faded)
    (set-face 'custom-changed                        'sumibi-face-salient)
    (set-face 'custom-modified                       'sumibi-face-salient)
    (set-face 'custom-face-tag                        'sumibi-face-strong)
    (set-face 'custom-variable-tag                    'sumibi-face-strong)
    (set-face 'custom-invalid                         'sumibi-face-popout)
    (set-face 'custom-visibility                     'sumibi-face-salient)
    (set-face 'custom-state                          'sumibi-face-salient)
    (set-face 'custom-link                           'sumibi-face-salient)))

(defun sumibi-theme--package ()
  "Derive package faces from sumibi faces."
  (with-eval-after-load 'package
    (set-face 'package-description                   'sumibi-face-default)
    (set-face 'package-help-section-name             'sumibi-face-default)
    (set-face 'package-name                          'sumibi-face-salient)
    (set-face 'package-status-avail-obso               'sumibi-face-faded)
    (set-face 'package-status-available              'sumibi-face-default)
    (set-face 'package-status-built-in               'sumibi-face-salient)
    (set-face 'package-status-dependency             'sumibi-face-salient)
    (set-face 'package-status-disabled                 'sumibi-face-faded)
    (set-face 'package-status-external               'sumibi-face-default)
    (set-face 'package-status-held                   'sumibi-face-default)
    (set-face 'package-status-incompat                 'sumibi-face-faded)
    (set-face 'package-status-installed              'sumibi-face-salient)
    (set-face 'package-status-new                    'sumibi-face-default)
    (set-face 'package-status-unsigned               'sumibi-face-default))

  ;; Button face is hardcoded, we have to redefine the relevant
  ;; function
  (defun package-make-button (text &rest properties)
    "Insert button labeled TEXT with button PROPERTIES at point.
PROPERTIES are passed to `insert-text-button', for which this
function is a convenience wrapper used by `describe-package-1'."
    (let ((button-text (if (display-graphic-p)
                           text (concat "[" text "]")))
          (button-face (if (display-graphic-p)
                           `(:box `(:line-width 1
                                    :color ,sumibi-color-subtle
                                    :style nil)
                                  :foreground ,sumibi-color-faded
                                  :background ,sumibi-color-subtle)
                         'link)))
      (apply #'insert-text-button button-text
               'face button-face 'follow-link t properties))))

(defun sumibi-theme--hydra ()
  "Derive hydra faces from sumibi faces."
  (with-eval-after-load 'hydra
    (set-face 'hydra-face-amaranth 'sumibi-face-strong)
    (set-face 'hydra-face-blue     'sumibi-face-strong)
    (set-face 'hydra-face-pink     'sumibi-face-strong)
    (set-face 'hydra-face-red      'sumibi-face-strong)
    (set-face 'hydra-face-teal     'sumibi-face-strong)))

(defun sumibi-theme--flyspell ()
  "Derive flyspell faces from sumibi faces."
  (with-eval-after-load 'flyspell
    (set-face 'flyspell-duplicate                     'sumibi-face-default)
    (set-face-attribute 'flyspell-incorrect nil
                        :underline `(:style wave :color ,sumibi-color-critical)
                        :background sumibi-color-background)))

(defun sumibi-theme--parinfer-rust ()
  "Derive parinfer-rust faces from sumibi-faces"
  (with-eval-after-load 'parinfer-rust
    (set-face-attribute 'parinfer-rust-dim-parens nil
                        :background sumibi-color-background
                        :foreground sumibi-color-foreground)))


(defun sumibi-theme--rainbow-delimiters ()
  "Derive rainbow-delimiters faces from sumibi faces."
  (with-eval-after-load 'rainbow-delimiters
    (set-face-attribute 'rainbow-delimiters-depth-1-face nil
                        :foreground sumibi-color-strong
                        :weight 'bold)
    (set-face-attribute 'rainbow-delimiters-depth-2-face nil
                        :foreground sumibi-color-faded)
    (set-face-attribute 'rainbow-delimiters-depth-3-face nil
                        :foreground sumibi-color-faded)
    (set-face-attribute 'rainbow-delimiters-depth-4-face nil
                        :foreground sumibi-color-faded)
    (set-face-attribute 'rainbow-delimiters-depth-5-face nil
                        :foreground sumibi-color-faded)
    (set-face-attribute 'rainbow-delimiters-depth-6-face nil
                        :foreground sumibi-color-faded)
    (set-face-attribute 'rainbow-delimiters-depth-7-face nil
                        :foreground sumibi-color-faded)
    (set-face-attribute 'rainbow-delimiters-depth-8-face nil
                        :foreground sumibi-color-faded)
    (set-face-attribute 'rainbow-delimiters-depth-9-face nil
                        :foreground sumibi-color-faded)
    (set-face-attribute 'rainbow-delimiters-base-error-face nil
                        :foreground sumibi-color-critical
                        :background sumibi-color-background
                        :weight 'bold)))


(defun sumibi-theme--goggles ()
  "Derive goggles faces from sumibi faces"
  (with-eval-after-load 'goggles
    (set-face-attribute 'goggles-added nil
                        :background "#defbe6")
    (set-face-attribute 'goggles-changed nil
                        :background "#f1c21b")
    (set-face-attribute 'goggles-removed nil
                        :background "#fff1f1")))

(defun sumibi-theme--keycast ()
  "Derive keycast faces from sumibi faces"
  (with-eval-after-load 'keycast
    (set-face-attribute 'keycast-key nil
                        :background "#d0e2ff"
                        :foreground "#0f62fe"
                        :weight 'bold
                        :height 1.2
                        :box nil)))

(defun sumibi-theme--eros ()
  "Derive eros face from sumibi faces"
  (with-eval-after-load 'eros
    (set-face-attribute 'eros-result-overlay-face nil
			            :background "#defbe6"
                        :foreground "#198038"
                        :weight 'bold
                        :box nil)))

(defun sumibi-theme--cider-overlay ()
  "Derive cider-overlay face from sumibi faces"
  (with-eval-after-load 'cider
    (set-face-attribute 'cider-result-overlay-face nil
			            :background "#defbe6"
                        :foreground "#198038"
                        :weight 'bold
                        :box nil)))


(defun sumibi-theme--flycheck ()
  "Derive flycheck faces from sumibi faces."
  (with-eval-after-load 'flycheck
    (set-face-attribute 'flycheck-error nil
                        :underline `(:style wave :color ,sumibi-color-critical)
                        :background sumibi-color-background)
    (set-face-attribute 'flycheck-warning nil
                        :underline `(:style wave :color ,sumibi-color-critical)
                        :background sumibi-color-background)
    (set-face-attribute 'flycheck-info  nil
                        :underline `(:style wave :color ,sumibi-color-faded)
                        :background sumibi-color-background)
    (set-face-attribute 'flycheck-error-list-error  nil
                        :background sumibi-color-background
                        :foreground sumibi-color-critical)))

(defun sumibi-theme--lsp ()
  "Derive flycheck faces from sumibi faces."
  (with-eval-after-load 'lsp-mode
    (set-face-attribute 'lsp-signature-posframe nil
                        :background sumibi-color-background
                        :foreground sumibi-color-foreground)))

(defun sumibi-theme--lsp-modeline ()
  "Derive lsp-code-actions faces from sumibi faces."
  (with-eval-after-load 'lsp-modeline
    (set-face-attribute 'lsp-modeline-code-actions-face nil
                        :background sumibi-color-background
                        :foreground "#198038")
    (set-face-attribute 'lsp-modeline-code-actions-preferred-face nil
                        :background sumibi-color-background
                        :foreground sumibi-color-critical)))

(defun sumibi-theme--lsp-headerline ()
  "Derive lsp-headerline faces from sumibi faces."
  (with-eval-after-load 'lsp-headerline
    (set-face-attribute 'lsp-headerline-breadcrumb-deprecated-face nil
                        :background sumibi-color-background
                        :foreground sumibi-color-critical
                        :weight 'bold)
    (set-face-attribute 'lsp-headerline-breadcrumb-path-error-face nil
                        :background sumibi-color-background
                        :foreground sumibi-color-critical
                        :weight 'bold)
    (set-face-attribute 'lsp-headerline-breadcrumb-path-face nil
                        :background sumibi-color-background
                        :foreground sumibi-color-foreground)
    (set-face-attribute 'lsp-headerline-breadcrumb-path-hint-face nil
                        :background sumibi-color-background
                        :foreground sumibi-color-foreground
                        :underline `(:style wave :color "#198038"))
    (set-face-attribute 'lsp-headerline-breadcrumb-path-info-face nil
                        :background sumibi-color-background
                        :foreground sumibi-color-foreground
                        :underline `(:style wave :color "#198038"))
    (set-face-attribute 'lsp-headerline-breadcrumb-path-warning-face nil
                        :background sumibi-color-background
                        :foreground sumibi-color-foreground
                        :underline `(:style wave :color "#f1c21b"))
    (set-face-attribute 'lsp-headerline-breadcrumb-project-prefix-face nil
                        :background sumibi-color-background
                        :foreground sumibi-color-foreground
                        :weight 'bold)
    (set-face-attribute 'lsp-headerline-breadcrumb-symbols-face nil
                        :background sumibi-color-background
                        :foreground sumibi-color-foreground)
    (set-face-attribute 'lsp-headerline-breadcrumb-symbols-error-face nil
                        :background sumibi-color-background
                        :foreground sumibi-color-critical
                        :weight 'bold)
    (set-face-attribute 'lsp-headerline-breadcrumb-symbols-hint-face nil
                        :background sumibi-color-background
                        :foreground sumibi-color-foreground
                        :underline `(:style wave :color "#198038"))
    (set-face-attribute 'lsp-headerline-breadcrumb-symbols-info-face nil
                        :background sumibi-color-background
                        :foreground sumibi-color-foreground
                        :underline `(:style wave :color "#198038"))
    (set-face-attribute 'lsp-headerline-breadcrumb-symbols-warning-face nil
                        :background sumibi-color-background
                        :foreground sumibi-color-foreground
                        :underline `(:style wave :color "#f1c21b"))))

(defun sumibi-theme--company ()
  "Derive company faces from sumibi faces."
  (with-eval-after-load 'company
    (set-face 'company-echo-common              'sumibi-face-faded)
    (set-face-attribute 'company-preview-common  nil
                        :background "#ffffff")
    (set-face 'company-preview-search           'sumibi-face-faded)
    (set-face-attribute 'company-scrollbar-bg nil
                        :background "#ffffff")
    (set-face-attribute 'company-scrollbar-fg nil
                        :background "#ffffff")
    (set-face 'company-tooltip-annotation       'sumibi-face-faded)
    (set-face 'company-tooltip-common           'sumibi-face-faded)
    (set-face-attribute 'company-tooltip-common-selection nil
                        :background "#ffffff"
                        :foreground sumibi-color-popout
                        :weight 'bold)
    (set-face-attribute 'company-tooltip-selection nil
                        :background "#ffffff"
                        :foreground sumibi-color-popout)
    (set-face-attribute 'company-preview nil
                        :background "#ffffff"
                        :foreground sumibi-color-foreground)
    (set-face-attribute 'company-tooltip nil
                        :background "#ffffff"
                        :foreground sumibi-color-foreground)))

(defun sumibi-theme--ctrlf ()
  "Derive ctrlf faces from sumibi faces."
  (with-eval-after-load 'ctrlf
    (set-face-attribute 'ctrlf-highlight-active nil
                        :background "#ffd6e8"
                        :foreground sumibi-color-foreground
                        :box `(:line-width 1
                               :color "#d02670"
                               :style nil))
    (set-face-attribute 'ctrlf-highlight-line nil
                        :background sumibi-color-highlight)
    (set-face-attribute 'ctrlf-highlight-passive nil
                        :background sumibi-color-background
                        :foreground sumibi-color-foreground
                        :box `(:line-width 1
                               :color "#2a0a18"
                               :style nil))))

(defun sumibi-theme--numbers-mode ()
  "Derive highlight-numbers-mode faces from sumibi face."
  (with-eval-after-load 'highlight-numbers
    (set-face-attribute 'highlight-numbers-number nil
                        :background sumibi-color-background
                        :foreground sumibi-color-numbers)))


(defun sumibi-theme--ido ()
  "Derive ido faces from sumibi faces."
  (with-eval-after-load 'ido
    (set-face 'ido-first-match                       'sumibi-face-salient)
    (set-face 'ido-only-match                          'sumibi-face-faded)
    (set-face 'ido-subdir                             'sumibi-face-strong)))

(defun sumibi-theme--diff ()
  "Derive diff faces from sumibi faces."
  (with-eval-after-load 'diff-mode
    (set-face 'diff-header                             'sumibi-face-faded)
    (set-face 'diff-file-header                       'sumibi-face-strong)
    (set-face 'diff-context                          'sumibi-face-default)
    (set-face 'diff-removed                            'sumibi-face-faded)
    (set-face 'diff-changed                           'sumibi-face-popout)
    (set-face 'diff-added                            'sumibi-face-salient)
    (set-face 'diff-refine-added                    '(sumibi-face-salient
                                                      sumibi-face-strong))
    (set-face 'diff-refine-changed                    'sumibi-face-popout)
    (set-face 'diff-refine-removed                    'sumibi-face-faded)
    (set-face-attribute     'diff-refine-removed nil :strike-through t)))


(defun sumibi-theme--term ()
  "Derive term faces from sumibi faces, and material theme colors."
  (with-eval-after-load 'term
    ;; (setq eterm-256color-disable-bold nil)
    (set-face 'term-bold                              'sumibi-face-strong)
    (set-face-attribute 'term-color-black nil
                         :foreground (face-foreground 'sumibi-face-default)
                         :background (face-foreground 'sumibi-face-default))
    (set-face-attribute 'term-color-white nil
                         :foreground (face-background 'sumibi-face-default)
                         :background (face-background 'sumibi-face-default))
    (set-face-attribute 'term-color-blue nil
                         :foreground sumibi-color-foreground
                         :background sumibi-color-foreground)
    (set-face-attribute 'term-color-cyan nil
                         :foreground sumibi-color-foreground
                         :background sumibi-color-foreground)
    (set-face-attribute 'term-color-green nil
                         :foreground sumibi-color-salient
                         :background sumibi-color-salient)
    (set-face-attribute 'term-color-magenta nil
                         :foreground sumibi-color-salient
                         :background sumibi-color-salient)
    (set-face-attribute 'term-color-red nil
                         :foreground sumibi-color-critical
                         :background sumibi-color-critical)
    (set-face-attribute 'term-color-yellow nil
                         :foreground sumibi-color-critical
                         :background sumibi-color-critical)))

(defun sumibi-theme--calendar ()
  "Derive calendar faces from sumibi faces."
  (with-eval-after-load 'calendar
    (set-face 'calendar-today                         'sumibi-face-strong)))


(defun sumibi-theme--agenda ()
  "Derive agenda faces from sumibi faces."
  (with-eval-after-load 'org-agenda
    (set-face 'org-agenda-calendar-event             'sumibi-face-default)
    (set-face 'org-agenda-calendar-sexp              'sumibi-face-salient)
    (set-face 'org-agenda-clocking                     'sumibi-face-faded)
    (set-face 'org-agenda-column-dateline              'sumibi-face-faded)
    (set-face 'org-agenda-current-time                'sumibi-face-strong)
    (set-face 'org-agenda-date                       'sumibi-face-salient)
    (set-face 'org-agenda-date-today                '(sumibi-face-salient
                                                       sumibi-face-strong))
    (set-face 'org-agenda-date-weekend                 'sumibi-face-faded)
    (set-face 'org-agenda-diary                        'sumibi-face-faded)
    (set-face 'org-agenda-dimmed-todo-face             'sumibi-face-faded)
    (set-face 'org-agenda-done                         'sumibi-face-faded)
    (set-face 'org-agenda-filter-category              'sumibi-face-faded)
    (set-face 'org-agenda-filter-effort                'sumibi-face-faded)
    (set-face 'org-agenda-filter-regexp                'sumibi-face-faded)
    (set-face 'org-agenda-filter-tags                  'sumibi-face-faded)
    ;;  (set-face 'org-agenda-property-face                'sumibi-face-faded)
    (set-face 'org-agenda-restriction-lock             'sumibi-face-faded)
    (set-face 'org-agenda-structure                   'sumibi-face-strong)))


(defun sumibi-theme--org ()
  "Derive org faces from sumibi faces."
  (with-eval-after-load 'org
    (set-face 'org-archived                            'sumibi-face-faded)
    (set-face 'org-block                                       'hl-line)
    (set-face 'org-block-begin-line                    'sumibi-face-faded)
    (set-face 'org-block-end-line                      'sumibi-face-faded)
    (unless (version< emacs-version "27.0")
      (set-face-attribute 'org-block nil                      :extend t)
      (set-face-attribute 'org-block-begin-line nil           :extend t)
      (set-face-attribute 'org-block-end-line nil             :extend t))
    (set-face 'org-checkbox                            'sumibi-face-faded)
    (set-face 'org-checkbox-statistics-done            'sumibi-face-faded)
    (set-face 'org-checkbox-statistics-todo            'sumibi-face-faded)
    (set-face 'org-clock-overlay                       'sumibi-face-faded)
    (set-face 'org-code                                'sumibi-face-faded)
    (set-face 'org-column                              'sumibi-face-faded)
    (set-face 'org-column-title                        'sumibi-face-faded)
    (set-face 'org-date                                'sumibi-face-faded)
    (set-face 'org-date-selected                       'sumibi-face-faded)
    (set-face 'org-default                             'sumibi-face-faded)
    (set-face 'org-document-info                       'sumibi-face-faded)
    (set-face 'org-document-info-keyword               'sumibi-face-faded)
    (set-face 'org-document-title                      'sumibi-face-faded)
    (set-face 'org-done                              'sumibi-face-default)
    (set-face 'org-drawer                              'sumibi-face-faded)
    (set-face 'org-ellipsis                            'sumibi-face-faded)
    (set-face 'org-footnote                            'sumibi-face-faded)
    (set-face 'org-formula                             'sumibi-face-faded)
    (set-face 'org-headline-done                       'sumibi-face-faded)
    (set-face 'org-latex-and-related                   'sumibi-face-faded)
    (set-face 'org-level-1                            'sumibi-face-strong)
    (set-face 'org-level-2                            'sumibi-face-strong)
    (set-face 'org-level-3                            'sumibi-face-strong)
    (set-face 'org-level-4                            'sumibi-face-strong)
    (set-face 'org-level-5                            'sumibi-face-strong)
    (set-face 'org-level-6                            'sumibi-face-strong)
    (set-face 'org-level-7                            'sumibi-face-strong)
    (set-face 'org-level-8                            'sumibi-face-strong)
    (set-face 'org-link                              'sumibi-face-salient)
    (set-face 'org-list-dt                             'sumibi-face-faded)
    (set-face 'org-macro                               'sumibi-face-faded)
    (set-face 'org-meta-line                           'sumibi-face-faded)
    (set-face 'org-mode-line-clock                     'sumibi-face-faded)
    (set-face 'org-mode-line-clock-overrun             'sumibi-face-faded)
    (set-face 'org-priority                            'sumibi-face-faded)
    (set-face 'org-property-value                      'sumibi-face-faded)
    (set-face 'org-quote                               'sumibi-face-faded)
    (set-face 'org-scheduled                           'sumibi-face-faded)
    (set-face 'org-scheduled-previously                'sumibi-face-faded)
    (set-face 'org-scheduled-today                     'sumibi-face-faded)
    (set-face 'org-sexp-date                           'sumibi-face-faded)
    (set-face 'org-special-keyword                     'sumibi-face-faded)
    (set-face 'org-table                               'sumibi-face-faded)
    (set-face 'org-tag                                'sumibi-face-popout)
    (set-face 'org-tag-group                           'sumibi-face-faded)
    (set-face 'org-target                              'sumibi-face-faded)
    (set-face 'org-time-grid                           'sumibi-face-faded)
    (set-face 'org-todo                              'sumibi-face-salient)
    (set-face 'org-upcoming-deadline                 'sumibi-face-default)
    (set-face 'org-verbatim                           'sumibi-face-popout)
    (set-face 'org-verse                               'sumibi-face-faded)
    (set-face 'org-warning                            'sumibi-face-popout)))

(defun sumibi-theme--dired ()
  "Derive dired faces from sumibi faces"
  (with-eval-after-load 'dired
    (set-face-attribute 'dired-header nil
                        :background sumibi-color-background
                        :foreground sumibi-color-foreground
                        :weight 'bold)
    (set-face-attribute 'dired-flagged nil
                        :background sumibi-color-background
                        :foreground sumibi-color-critical
                        :weight 'bold)
    (set-face-attribute 'dired-warning nil
                        :background sumibi-color-background
                        :foreground sumibi-color-critical
                        :weight 'bold)
    (set-face-attribute 'dired-set-id nil
                        :background sumibi-color-background
                        :foreground sumibi-color-critical
                        :weight 'bold)))


(defun sumibi-theme--rst ()
  "Derive rst faces from sumibi faces."
  (with-eval-after-load 'rst
    (set-face 'rst-adornment                           'sumibi-face-faded)
    (set-face 'rst-block                             'sumibi-face-default)
    (set-face 'rst-comment                             'sumibi-face-faded)
    (set-face 'rst-definition                        'sumibi-face-salient)
    (set-face 'rst-directive                         'sumibi-face-salient)
    (set-face 'rst-emphasis1                           'sumibi-face-faded)
    (set-face 'rst-emphasis2                          'sumibi-face-strong)
    (set-face 'rst-external                          'sumibi-face-salient)
    (set-face 'rst-level-1                            'sumibi-face-strong)
    (set-face 'rst-level-2                            'sumibi-face-strong)
    (set-face 'rst-level-3                            'sumibi-face-strong)
    (set-face 'rst-level-4                            'sumibi-face-strong)
    (set-face 'rst-level-5                            'sumibi-face-strong)
    (set-face 'rst-level-6                            'sumibi-face-strong)
    (set-face 'rst-literal                           'sumibi-face-salient)
    (set-face 'rst-reference                         'sumibi-face-salient)
    (set-face 'rst-transition                        'sumibi-face-default)))


(defun sumibi-theme--markdown ()
  "Derive markdown faces from sumibi faces."
  (with-eval-after-load 'markdown-mode
    (set-face 'markdown-blockquote-face              'sumibi-face-default)
    (set-face 'markdown-bold-face                     'sumibi-face-strong)
    (set-face 'markdown-code-face                    'sumibi-face-default)
    (set-face 'markdown-comment-face                   'sumibi-face-faded)
    (set-face 'markdown-footnote-marker-face         'sumibi-face-default)
    (set-face 'markdown-footnote-text-face           'sumibi-face-default)
    (set-face 'markdown-gfm-checkbox-face            'sumibi-face-default)
    (set-face 'markdown-header-delimiter-face          'sumibi-face-faded)
    (set-face 'markdown-header-face                   'sumibi-face-strong)
    (set-face 'markdown-header-face-1                 'sumibi-face-strong)
    (set-face 'markdown-header-face-2                 'sumibi-face-strong)
    (set-face 'markdown-header-face-3                 'sumibi-face-strong)
    (set-face 'markdown-header-face-4                 'sumibi-face-strong)
    (set-face 'markdown-header-face-5                 'sumibi-face-strong)
    (set-face 'markdown-header-face-6                'sumibi-face-strong)
    (set-face 'markdown-header-rule-face             'sumibi-face-default)
    (set-face 'markdown-highlight-face               'sumibi-face-default)
    (set-face 'markdown-hr-face                      'sumibi-face-default)
    (set-face 'markdown-html-attr-name-face          'sumibi-face-default)
    (set-face 'markdown-html-attr-value-face         'sumibi-face-default)
    (set-face 'markdown-html-entity-face             'sumibi-face-default)
    (set-face 'markdown-html-tag-delimiter-face      'sumibi-face-default)
    (set-face 'markdown-html-tag-name-face           'sumibi-face-default)
    (set-face 'markdown-inline-code-face              'sumibi-face-popout)
    (set-face 'markdown-italic-face                    'sumibi-face-faded)
    (set-face 'markdown-language-info-face           'sumibi-face-default)
    (set-face 'markdown-language-keyword-face        'sumibi-face-default)
    (set-face 'markdown-line-break-face              'sumibi-face-default)
    (set-face 'markdown-link-face                    'sumibi-face-salient)
    (set-face 'markdown-link-title-face              'sumibi-face-default)
    (set-face 'markdown-list-face                      'sumibi-face-faded)
    (set-face 'markdown-markup-face                    'sumibi-face-faded)
    (set-face 'markdown-math-face                    'sumibi-face-default)
    (set-face 'markdown-metadata-key-face              'sumibi-face-faded)
    (set-face 'markdown-metadata-value-face            'sumibi-face-faded)
    (set-face 'markdown-missing-link-face            'sumibi-face-default)
    (set-face 'markdown-plain-url-face               'sumibi-face-default)
    (set-face 'markdown-pre-face                     'sumibi-face-default)
    (set-face 'markdown-reference-face               'sumibi-face-salient)
    (set-face 'markdown-strike-through-face            'sumibi-face-faded)
    (set-face 'markdown-table-face                   'sumibi-face-default)
    (set-face 'markdown-url-face                     'sumibi-face-salient)))


(defun sumibi-theme--ivy ()
  "Derive ivy faces from sumibi faces."
  (with-eval-after-load 'ivy
    (set-face 'ivy-action                              'sumibi-face-faded)
    (set-face 'ivy-completions-annotations             'sumibi-face-faded)
    (set-face 'ivy-confirm-face                        'sumibi-face-faded)
    (set-face 'ivy-current-match    '(sumibi-face-strong sumibi-face-subtle))
    (set-face 'ivy-cursor                             'sumibi-face-strong)
    (set-face 'ivy-grep-info                          'sumibi-face-strong)
    (set-face 'ivy-grep-line-number                    'sumibi-face-faded)
    (set-face 'ivy-highlight-face                     'sumibi-face-strong)
    (set-face 'ivy-match-required-face                 'sumibi-face-faded)
    (set-face 'ivy-minibuffer-match-face-1             'sumibi-face-faded)
    (set-face 'ivy-minibuffer-match-face-2             'sumibi-face-faded)
    (set-face 'ivy-minibuffer-match-face-3             'sumibi-face-faded)
    (set-face 'ivy-minibuffer-match-face-4             'sumibi-face-faded)
    (set-face 'ivy-minibuffer-match-highlight         'sumibi-face-strong)
    (set-face 'ivy-modified-buffer                    'sumibi-face-popout)
    (set-face 'ivy-modified-outside-buffer            'sumibi-face-strong)
    (set-face 'ivy-org                                 'sumibi-face-faded)
    (set-face 'ivy-prompt-match                        'sumibi-face-faded)
    (set-face 'ivy-remote                            'sumibi-face-default)
    (set-face 'ivy-separator                           'sumibi-face-faded)
    (set-face 'ivy-subdir                              'sumibi-face-faded)
    (set-face 'ivy-virtual                             'sumibi-face-faded)
    (set-face 'ivy-yanked-word                         'sumibi-face-faded)))

(defun sumibi-theme--helm ()
  "Derive helm faces from sumibi faces."
  (with-eval-after-load 'helm
    (set-face 'helm-selection                '(sumibi-face-strong sumibi-face-subtle))
    (set-face 'helm-match                    'sumibi-face-strong)
    (set-face 'helm-source-header            'sumibi-face-salient)
    (set-face 'helm-visible-mark             'sumibi-face-strong)))

(defun sumibi-theme--helm-swoop ()
  "Derive helm faces from sumibi faces."
  (with-eval-after-load 'helm-swoop
    (set-face 'helm-swoop-target-line-face   '(sumibi-face-strong sumibi-face-subtle))))

(defun sumibi-theme--helm-occur ()
  "Derive helm faces from sumibi faces."
  (with-eval-after-load 'helm-occur
    (set-face 'helm-moccur-buffer             'sumibi-face-strong)))

(defun sumibi-theme--helm-ff ()
  "Derive helm faces from sumibi faces."
  (with-eval-after-load 'helm-ff
    (set-face 'helm-ff-file                 'sumibi-face-faded)
    (set-face 'helm-ff-prefix               'sumibi-face-strong)
    (set-face 'helm-ff-dotted-directory     'sumibi-face-faded)
    (set-face 'helm-ff-directory            'sumibi-face-strong)
    (set-face 'helm-ff-executable           'sumibi-face-popout)))

(defun sumibi-theme--helm-grep ()
  "Derive helm faces from sumibi faces."
  (with-eval-after-load 'helm-grep
    (set-face 'helm-grep-match                        'sumibi-face-strong)
    (set-face 'helm-grep-file                         'sumibi-face-faded)
    (set-face 'helm-grep-lineno                       'sumibi-face-faded)
    (set-face 'helm-grep-finish                       'sumibi-face-default)))

(defun sumibi-theme ()
  "Derive many, many faces from the core sumibi faces."
  (sumibi-theme--agenda)
  (sumibi-theme--basics)
  (sumibi-theme--bookmark)
  (sumibi-theme--buttons)
  (sumibi-theme--calendar)
  (sumibi-theme--company)
  (sumibi-theme--ctrlf)
  (sumibi-theme--customize)
  (sumibi-theme--diff)
  (sumibi-theme--flycheck)
  (sumibi-theme--flyspell)
  (sumibi-theme--font-lock)
  (sumibi-theme--helm)
  (sumibi-theme--helm-ff)
  (sumibi-theme--helm-grep)
  (sumibi-theme--helm-occur)
  (sumibi-theme--helm-swoop)
  (sumibi-theme--rainbow-delimiters)
  (sumibi-theme--hl-line)
  (sumibi-theme--ido)
  (sumibi-theme--info)
  (sumibi-theme--ivy)
  (sumibi-theme--markdown)
  (sumibi-theme--message)
  (sumibi-theme--minibuffer)
  (sumibi-theme--parinfer-rust)
  (sumibi-theme--dired)
  (sumibi-theme--mode-line)
  (sumibi-theme--org)
  (sumibi-theme--outline)
  (sumibi-theme--package)
  (sumibi-theme--eros)
  (sumibi-theme--cider-overlay)
  (sumibi-theme--hydra)
  (sumibi-theme--goggles)
  (sumibi-theme--numbers-mode)
  (sumibi-theme--rst)
  (sumibi-theme--keycast)
  (sumibi-theme--lsp)
  (sumibi-theme--lsp-modeline)
  (sumibi-theme--lsp-headerline)
  (sumibi-theme--speedbar)
  (sumibi-theme--term))
 
(provide 'sumibi-theme)

;;; sumibi-theme.el ends here