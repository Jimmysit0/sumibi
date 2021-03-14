;; -*- lexical-binding: t; -*-
;; sumibi-theme.el
;; basically load themes :)
;;; Code:

(require 'sumibi-faces)

;; When we set a face, we take care of removing any previous settings
(defun set-face (face style)
  "Reset a face and make it inherit style."
  (set-face-attribute face nil
   :foreground 'unspecified :background 'unspecified
   :family     'unspecified :slant      'unspecified
   :weight     'unspecified :height     'unspecified
   :underline  'unspecified :overline   'unspecified
   :box        'unspecified :inherit    style))

;; Structural
(set-face 'bold                                     'sumibi-face-strong)
(set-face 'italic                                    'sumibi-face-faded)
(set-face 'bold-italic                              'sumibi-face-strong)
(set-face 'region                                   'sumibi-face-subtle)
(set-face 'highlight                                'sumibi-face-subtle)
(set-face 'fixed-pitch-serif                       'sumibi-face-default)
(set-face 'variable-pitch                          'sumibi-face-default)
(set-face 'cursor                                  'sumibi-face-default)

(set-face-attribute 'cursor nil
                                        :background (face-foreground 'sumibi-face-cursor))
(set-face-attribute 'window-divider nil
                                        :foreground (face-background 'sumibi-face-default))
(set-face-attribute 'window-divider-first-pixel nil
                                        :foreground sumibi-color-highlight)
(set-face-attribute 'window-divider-last-pixel nil
                                        :foreground sumibi-color-highlight)

;; Minibuffer / echo area
(dolist (buffer (list " *Minibuf-0*" " *Echo Area 0*"
                      " *Minibuf-1*" " *Echo Area 1*"))
  (when (get-buffer buffer)
    (with-current-buffer buffer
      (face-remap-add-relative 'default 'sumibi-face-faded))))


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
(set-face 'show-paren-match                         'sumibi-face-popout)
(set-face 'show-paren-mismatch                           'face-normal)
(set-face-attribute 'tooltip nil                         :height 0.85)
(set-face 'secondary-selection                      'sumibi-face-subtle)
(set-face 'completions-common-part                  'sumibi-face-faded)
(set-face 'completions-first-difference             'sumibi-face-popout)

;; Programmation mode
(set-face 'font-lock-comment-face                    'sumibi-face-faded)
(set-face 'font-lock-doc-face                        'sumibi-face-faded)
(set-face 'font-lock-string-face                    'sumibi-face-popout)
(set-face 'font-lock-constant-face                 'sumibi-face-salient)
(set-face 'font-lock-warning-face                   'sumibi-face-popout)
(set-face 'font-lock-function-name-face             'sumibi-face-strong)
(set-face 'font-lock-variable-name-face             'sumibi-face-strong)
(set-face 'font-lock-builtin-face                  'sumibi-face-salient)
(set-face 'font-lock-type-face                     'sumibi-face-salient)
(set-face 'font-lock-keyword-face                  'sumibi-face-salient)


;; Highlight line mode
(with-eval-after-load 'hl-line
  (set-face-attribute 'hl-line nil
                                          :background sumibi-color-highlight))

;; Buttons
(with-eval-after-load 'cus-edit
  (set-face-attribute 'custom-button nil
                                          :foreground (face-foreground 'sumibi-face-faded)
                                          :background (face-background 'sumibi-face-default)
                                          :box `(:line-width 1
                                                         :color ,(face-foreground 'sumibi-face-faded)
                                                         :style nil))
  (set-face-attribute 'custom-button-mouse nil
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
                                          :inverse-video nil))

;; Documentation
(with-eval-after-load 'info
  (set-face 'info-menu-header                       'sumibi-face-strong)
  (set-face 'info-header-node                      'sumibi-face-default)
  (set-face 'info-index-match                      'sumibi-face-salient)
  (set-face 'Info-quoted                             'sumibi-face-faded)
  (set-face 'info-title-1                           'sumibi-face-strong)
  (set-face 'info-title-2                           'sumibi-face-strong)
  (set-face 'info-title-3                           'sumibi-face-strong)
  (set-face 'info-title-4                           'sumibi-face-strong))

;; Bookmarks
(with-eval-after-load 'bookmark
  (set-face 'bookmark-menu-heading                  'sumibi-face-strong)
  (set-face 'bookmark-menu-bookmark                'sumibi-face-salient))

;; Message
(with-eval-after-load 'message
  (set-face 'message-cited-text                      'sumibi-face-faded)
  (set-face 'message-cited-text-1                    'sumibi-face-faded)
  (set-face 'message-cited-text-2                    'sumibi-face-faded)
  (set-face 'message-cited-text-3                    'sumibi-face-faded)
  (set-face 'message-cited-text-4                    'sumibi-face-faded)
  (set-face 'message-header-cc                     'sumibi-face-default)
  (set-face 'message-header-name                    'sumibi-face-strong)
  (set-face 'message-header-newsgroups             'sumibi-face-default)
  (set-face 'message-header-other                  'sumibi-face-default)
  (set-face 'message-header-subject                'sumibi-face-salient)
  (set-face 'message-header-to                     'sumibi-face-salient)
  (set-face 'message-header-xheader                'sumibi-face-default)
  (set-face 'message-mml                            'sumibi-face-popout)
  (set-face 'message-separator                       'sumibi-face-faded))

;; Outline
(with-eval-after-load 'outline
  (set-face 'outline-1                              'sumibi-face-strong)
  (set-face 'outline-2                              'sumibi-face-strong)
  (set-face 'outline-3                              'sumibi-face-strong)
  (set-face 'outline-4                              'sumibi-face-strong)
  (set-face 'outline-5                              'sumibi-face-strong)
  (set-face 'outline-6                              'sumibi-face-strong)
  (set-face 'outline-7                              'sumibi-face-strong)
  (set-face 'outline-8                              'sumibi-face-strong))

;; Interface
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
  (set-face 'custom-variable-tag                   'sumibi-face-default)
  (set-face 'custom-invalid                         'sumibi-face-popout)
  (set-face 'custom-visibility                     'sumibi-face-salient)
  (set-face 'custom-state                          'sumibi-face-salient)
  (set-face 'custom-link                           'sumibi-face-salient))

;; Package
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
  (set-face 'package-status-unsigned               'sumibi-face-default)

  ;; Button face is hardcoded, we have to redefine the relevant
  ;; function
  (defun package-make-button (text &rest properties)
        "Insert button labeled TEXT with button PROPERTIES at point.
PROPERTIES are passed to `insert-text-button', for which this
function is a convenience wrapper used by `describe-package-1'."
        (let ((button-text (if (display-graphic-p)
                                                   text (concat "[" text "]")))
                  (button-face (if (display-graphic-p)
                                                   '(:box `(:line-width 1
                                                                        :color ,sumibi-color-subtle
                                                                        :style nil)
                                                                  :foreground sumibi-color-faded
                                                                  :background sumibi-color-subtle)
                                                 'link)))
          (apply #'insert-text-button button-text
                         'face button-face 'follow-link t properties))))

;; Flyspell
(with-eval-after-load 'flyspell
  (set-face 'flyspell-duplicate                     'sumibi-face-popout)
  (set-face 'flyspell-incorrect                     'sumibi-face-popout))

;; Ido
(with-eval-after-load 'ido
  (set-face 'ido-first-match                       'sumibi-face-salient)
  (set-face 'ido-only-match                          'sumibi-face-faded)
  (set-face 'ido-subdir                             'sumibi-face-strong))

;; Diff
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
  (set-face-attribute     'diff-refine-removed nil :strike-through t))

;; Term
(with-eval-after-load 'term
  ;; (setq eterm-256color-disable-bold nil)
  (set-face 'term-bold                                   'sumibi-face-strong)
  (set-face-attribute 'term-color-black nil
                                          :foreground (face-foreground 'sumibi-face-default)
                                          :background (face-foreground 'sumibi-face-default))
  (set-face-attribute 'term-color-white nil
                                          :foreground (face-background 'sumibi-face-default)
                                          :background (face-background 'sumibi-face-default))
  (set-face-attribute 'term-color-blue nil
                                          :foreground "#42A5F5"   ;; material color blue L400
                                          :background "#BBDEFB")  ;; material color blue L100
  (set-face-attribute 'term-color-cyan nil
                                          :foreground "#26C6DA"   ;; material color cyan L400
                                          :background "#B2EBF2")  ;; material color cyan L100
  (set-face-attribute 'term-color-green nil
                                          :foreground "#66BB6A"   ;; material color green L400
                                          :background "#C8E6C9")  ;; material color green L100
  (set-face-attribute 'term-color-magenta nil
                                          :foreground "#AB47BC"   ;; material color purple L400
                                          :background "#E1BEE7")  ;; material color purple L100
  (set-face-attribute 'term-color-red nil
                                          :foreground "#EF5350"   ;; material color red L400
                                          :background "#FFCDD2")  ;; material color red L100
  (set-face-attribute 'term-color-yellow nil
                                          :foreground "#FFEE58"   ;; material color yellow L400
                                          :background "#FFF9C4")) ;; material color yellow L100

(with-eval-after-load 'calendar
  (set-face 'calendar-today                         'sumibi-face-strong))

;; org-agenda
(with-eval-after-load 'org-agenda
  (set-face 'org-agenda-calendar-event             'sumibi-face-default)
  (set-face 'org-agenda-calendar-sexp              'sumibi-face-salient)
  (set-face 'org-agenda-clocking                     'sumibi-face-faded)
  (set-face 'org-agenda-column-dateline              'sumibi-face-faded)
  (set-face 'org-agenda-current-time                'sumibi-face-strong)
  (set-face 'org-agenda-date                        'sumibi-face-salient)
  (set-face 'org-agenda-date-today                  '(sumibi-face-strong
   sumibi-face-salient))
  (set-face 'org-agenda-date-weekend                 'sumibi-face-faded)
  (set-face 'org-agenda-diary                        'sumibi-face-faded)
  (set-face 'org-agenda-dimmed-todo-face             'sumibi-face-faded)
  (set-face 'org-agenda-done                         'sumibi-face-faded)
  (set-face 'org-agenda-filter-category              'sumibi-face-faded)
  (set-face 'org-agenda-filter-effort                'sumibi-face-faded)
  (set-face 'org-agenda-filter-regexp                'sumibi-face-faded)
  (set-face 'org-agenda-filter-tags                  'sumibi-face-faded)
  (set-face 'org-agenda-restriction-lock             'sumibi-face-faded)
  (set-face 'org-agenda-structure                   'sumibi-face-strong))

;; org mode
(with-eval-after-load 'org
  (set-face 'org-archived                            'sumibi-face-faded)
  (set-face 'org-block                               'sumibi-face-faded)
  (set-face 'org-block-begin-line                    'sumibi-face-faded)
  (set-face 'org-block-end-line                      'sumibi-face-faded)
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
  (set-face 'org-level-3                           'sumibi-face-salient)
  (set-face 'org-level-4                           'sumibi-face-default)
  (set-face 'org-level-5                           'sumibi-face-default)
  (set-face 'org-level-6                           'sumibi-face-default)
  (set-face 'org-level-7                           'sumibi-face-default)
  (set-face 'org-level-8                           'sumibi-face-default)
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
  (set-face 'org-tag                                 'sumibi-face-faded)
  (set-face 'org-tag-group                           'sumibi-face-faded)
  (set-face 'org-target                              'sumibi-face-faded)
  (set-face 'org-time-grid                           'sumibi-face-faded)
  (set-face 'org-todo                              'sumibi-face-salient)
  (set-face 'org-upcoming-deadline                   'sumibi-face-faded)
  (set-face 'org-verbatim                            'sumibi-face-faded)
  (set-face 'org-verse                               'sumibi-face-faded)
  (set-face 'org-warning                            'sumibi-face-popout))

;; sumibieed
(with-eval-after-load 'sumibieed
  (set-face 'sumibieed-log-date-face                    'sumibi-face-faded)
  (set-face 'sumibieed-log-info-level-face            'sumibi-face-default)
  (set-face 'sumibieed-log-debug-level-face           'sumibi-face-default)
  (set-face 'sumibieed-log-warn-level-face             'sumibi-face-popout)
  (set-face 'sumibieed-log-error-level-face            'sumibi-face-popout)
  (set-face 'sumibieed-search-tag-face                  'sumibi-face-faded)
  (set-face 'sumibieed-search-date-face                 'sumibi-face-faded)
  (set-face 'sumibieed-search-feed-face               'sumibi-face-salient)
  (set-face 'sumibieed-search-filter-face               'sumibi-face-faded)
  (set-face 'sumibieed-search-last-update-face        'sumibi-face-salient)
  (set-face 'sumibieed-search-title-face              'sumibi-face-default)
  (set-face 'sumibieed-search-tag-face                  'sumibi-face-faded)
  (set-face 'sumibieed-search-unread-count-face        'sumibi-face-strong)
  (set-face 'sumibieed-search-unread-title-face        'sumibi-face-strong))

;; RST mode
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
  (set-face 'rst-transition                        'sumibi-face-default))

;; Markdown mode
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
  (set-face 'markdown-url-face                     'sumibi-face-salient))

;; Ivy
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
  (set-face 'ivy-minibuffer-match-face-1           'sumibi-face-salient)
  (set-face 'ivy-minibuffer-match-face-2           'sumibi-face-salient)
  (set-face 'ivy-minibuffer-match-face-3           'sumibi-face-salient)
  (set-face 'ivy-minibuffer-match-face-4           'sumibi-face-salient)
  (set-face 'ivy-minibuffer-match-highlight         'sumibi-face-strong)
  (set-face 'ivy-modified-buffer                     'sumibi-face-faded)
  (set-face 'ivy-modified-outside-buffer             'sumibi-face-faded)
  (set-face 'ivy-org                                 'sumibi-face-faded)
  (set-face 'ivy-prompt-match                        'sumibi-face-faded)
  (set-face 'ivy-remote                            'sumibi-face-default)
  (set-face 'ivy-separator                           'sumibi-face-faded)
  (set-face 'ivy-subdir                              'sumibi-face-faded)
  (set-face 'ivy-virtual                             'sumibi-face-faded)
  (set-face 'ivy-yanked-word                         'sumibi-face-faded)
)

(provide 'sumibi-theme)
