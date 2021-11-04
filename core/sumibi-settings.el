;;; sumibi-settings.el -*- lexical-binding: t; -*-

;; sumibi setting options

(defgroup sumibi nil
  "sumibi-emacs custom options."
  :group 'emacs)

(set-language-environment "UTF-8")

;; Replace the region while insert
(use-package delsel
  :straight nil
  :hook (after-init . delete-selection-mode))

;; simple.el
(use-package simple
  :straight nil
  :config
  (setq-default yank-pop-change-selection t
                ;; Disable key bindging suggeestions
                suggest-key-bindings t
                kill-whole-line t	; Kill line including '\n'
                eval-expression-print-level nil
                set-mark-command-repeat-pop t

                async-shell-command-buffer 'new-buffer
                backward-delete-char-untabify-method 'hungry

                track-eol t ; Keep cursor at end of lines.
                line-move-visual nil ; To be required by track-eol

                ;; Update UI less frequently
                idle-update-delay 1.0
                ;; TODO: move to jit-lock
                jit-lock-defer-time 0
                )
  )

;; Auto reload buffer if file was changed
(use-package autorevert
  :straight nil
  :hook (after-init . global-auto-revert-mode)
  :config
  (setq auto-revert-interval 10
        auto-revert-check-vc-info nil
        ;; global-auto-revert-non-file-buffers t
        ;; TODO: Try fix hangs:
        auto-revert-use-notify nil

        auto-revert-verbose nil))

;; Suppress ad-handle-definition warnings
(setq ad-redefinition-action 'accept)

;; Save last position in buffer
(use-package saveplace
  :straight nil
  :hook (after-init . save-place-mode))

;; File-related settings
(use-package files
  :straight nil
  :config
  (setq make-backup-files nil
        backup-by-copying t      ; don't clobber symlinks
        backup-by-copying-when-linked t
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t ; use versioned backups
        auto-save-default t

        large-file-warning-threshold nil ; Disable warning about large files
        require-final-newline t
        find-file-visit-truename t

        ;; Don’t bother confirming killing processes
        confirm-kill-processes nil

        ;; y/n instead of yes/no when quitting
        confirm-kill-emacs nil

        ;; No second pass of case-insensitive search over auto-mode-alist.
        auto-mode-case-fold nil
        ))

;; Allow commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(provide 'sumibi-settings)
