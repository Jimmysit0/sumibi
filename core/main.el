;; -- Importants to have

;; -*- lexical-binding: t; -*-

;; Detect system type
(defconst *IS-LINUX*   (eq system-type 'gnu/linux))
(defconst *IS-MAC*     (eq system-type 'darwin))

;; Remove command line options that aren't relevant to our current OS; means
;; slightly less to process at startup.
(unless *IS-MAC*   (setq command-line-ns-option-alist nil))
(unless *IS-LINUX* (setq command-line-x-option-alist nil))

(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil
      ring-bell-function 'ignore
      show-paren-delay 0
      x-underline-at-descent-line t)

(blink-cursor-mode -1)                            ; Do not blink cursor
(global-hl-line-mode 1)                           ; hl-line
(show-paren-mode 1)                               ; Highlight the match parentesis
(tooltip-mode 0)                                  ; Do not display tooltips
(auto-fill-mode nil)                              ; Please do not annoy me

(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 indent-tabs-mode nil                             ; No indent tabs
 uniquify-buffer-name-style 'forward              ; Uniquify buffer names
 window-combination-resize t                      ; Take new window space from all other windows
 frame-title-format  '("-")                       ; Window title formating
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width

(setq backup-directory-alist                      ; Backups files' home (they do not have)
          `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      inhibit-compacting-font-caches t            ; When there are lots of glyphs, keep them in memory
      truncate-string-ellipsis "…")               ; Unicode ellispis are nicer than "..."

(if (eq initial-window-system 'x)                 ; If started by emacs command or desktop file
        (toggle-frame-maximized)                  ; maximize window and ...
        (toggle-frame-fullscreen))                ; toggle fullscreen


(setq cursor-in-non-selected-windows nil)         ; No cursor in inactive windows
(setq initial-major-mode 'text-mode)              ; Text mode is initial mode
(setq default-major-mode 'text-mode)              ; Text mode is default major mode
(setq confirm-nonexistent-file-or-buffer nil)     ; No confirmation for visiting non-existent files
(setq-default indent-tabs-mode nil)               ; No tabs
(setq tab-width 4)                                ; Tab.space equivalence
(setq window-min-height 1)                        ; Minimum window height


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

;; Kill current buffer
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; Vertical window divider
(setq window-divider-default-right-width 16)
(setq window-divider-default-places 'right-only)
(window-divider-mode 1)

;; No ugly button for checkboxes
(setq widget-image-enable nil)


(provide 'main)       
