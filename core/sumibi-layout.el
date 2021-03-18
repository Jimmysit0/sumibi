;; -*- lexical-binding: t; -*-
;; layout.el
;;
;; This file defines some minor tweaks, the font used and so on
;;
;;; Code:

(setq default-frame-alist
      (append (list
               '(font . "Space Mono 14")
               '(min-height . 1)  '(height     . 45)
               '(min-width  . 40) '(width      . 81)
               '(vertical-scroll-bars . nil)
               '(internal-border-width . 24)
               '(left-fringe    . 0)
               '(right-fringe   . 0)
               '(tool-bar-lines . 0)
               '(menu-bar-lines . 0))))

;; Fall back font for glyph missing in Space Mono
(defface fallback '((t :family "Fira Code"
                       :inherit 'nano-face-faded)) "Fallback")
(set-display-table-slot standard-display-table 'truncation
                        (make-glyph-code ?… 'fallback))
(set-display-table-slot standard-display-table 'wrap
                        (make-glyph-code ?↩ 'fallback))

(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil)

;; Minor-tweaks
(setq ring-bell-function 'ignore
      show-paren-delay 0
      x-underline-at-descent-line t)

(defalias 'yes-or-no-p 'y-or-n-p)

(blink-cursor-mode -1)
(global-hl-line-mode 1)
(show-paren-mode 1)
(tooltip-mode 0)
(auto-fill-mode nil)

(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 indent-tabs-mode nil                             ; No indent tabs
 uniquify-buffer-name-style 'forward              ; Uniquify buffer names
 window-combination-resize t                      ; Take new window space from all other windows
 frame-title-format  '("sumibi")              ; Window title formating
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width

;; Smooth-scrolling
(setq redisplay-dont-pause t
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position t
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      jit-lock-defer-time 0)

(setq backup-directory-alist                      ; Backups files' home
          `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      inhibit-compacting-font-caches t            ; When there are lots of glyphs, keep them in memory
      truncate-string-ellipsis "…")               ; Unicode ellispis are nicer than "..."

(if (eq initial-window-system 'x)                 ; If started by emacs command or desktop file ...
        (toggle-frame-maximized)                  ; toggle fullscreen
        (toggle-frame-fullscreen))


;; Vertical window divider
(setq window-divider-default-right-width 24)
(setq window-divider-default-places 'right-only)
(window-divider-mode 1)

;; Creating a new window switches your cursor to it
; from: https://github.com/snackon/Witchmacs#creating-a-new-window-switches-your-cursor-to-it
(defun split-and-follow-window-h ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-window-h)

(defun split-and-follow-window-v ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-window-v)

(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; No ugly button for checkboxes
(setq widget-image-enable nil)

;; Hide org markup for README
(setq org-hide-emphasis-markers t)

(provide 'sumibi-layout)
