;;; Comment:
;;; This file have the configuration for LSP mode
;;; and a hook that uses writeroom-mode to hide the headerline
;;; to do not do weird stuff with the normal headerline and use the LSP headerline.


;;; Also LSP support for C/Cpp and Clojure
;;; Check LSP docs for more languages here: https://emacs-lsp.github.io/lsp-mode/page/languages/

;;; Code:

;; LSP and friends

(use-package lsp-mode
  :commands lsp
  :config
  (custom-set-faces
   '(lsp-dired-path-error-face ((t (:underline (:color "#B4637A" :style wave)))))
   '(lsp-dired-path-hint-face ((t (:underline (:color "#286983" :style wave)))))
   '(lsp-dired-path-info-face ((t (:underline (:color "#286983" :style wave)))))
   '(lsp-dired-path-warning-face ((t (:underline (:color "#E5E5E5" :style wave)))))
   '(lsp-headerline-breadcrumb-path-error-face
     ((t (:inherit lsp-headerline-breadcrumb-path-face :underline (:color "#B4637A" :style wave)))))
   '(lsp-headerline-breadcrumb-path-hint-face
     ((t (:inherit lsp-headerline-breadcrumb-path-face :underline (:color "#286983" :style wave)))))
   '(lsp-headerline-breadcrumb-path-info-face
     ((t (:inherit lsp-headerline-breadcrumb-path-face :underline (:color "#286983" :style wave)))))
   '(lsp-headerline-breadcrumb-path-warning-face
     ((t (:inherit lsp-headerline-breadcrumb-path-face :underline (:color "#E5E5E5" :style wave)))))
   '(lsp-headerline-breadcrumb-symbols-error-face
     ((t (:inherit lsp-headerline-breadcrumb-symbols-face :underline (:color "#B4637A" :style wave)))))
   '(lsp-headerline-breadcrumb-symbols-hint-face
     ((t (:inherit lsp-headerline-breadcrumb-symbols-face :underline (:color "#286983" :style wave)))))
   '(lsp-headerline-breadcrumb-symbols-info-face
     ((t (:inherit lsp-headerline-breadcrumb-symbols-face :underline (:color "#286983" :style wave)))))))

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (custom-set-faces
   '(lsp-ui-doc-background     ((t (:background "#F2E9DE"))))
   '(lsp-ui-doc-header         ((t (:background "#F2E9DE" :foreground "#6e6a86"))))
   '(lsp-ui-peek-filename      ((t (:foreground "#9893A5"))))
   '(lsp-ui-peek-header        ((t (:background "#F2E9DE" :foreground "#575279"))))
   '(lsp-ui-peek-highlight     ((t (:background "#F2E9DE" :box (:line-width (1 . -1) :color "#E5E5E5")))))
   '(lsp-ui-peek-list          ((t (:background "#F2E9DE"))))
   '(lsp-ui-peek-peek          ((t (:background "#F2E9DE"))))
   '(lsp-ui-peek-selection     ((t (:background "#F2E9DE" :foreground "#575279")))))
  (setq lsp-ui-sideline nil
        lsp-ui-doc-enable nil))

;;; --- flycheck --- this shouldn't be here, but I'm too lazy to move it :p

(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :config
  (custom-set-faces
   '(flycheck-error        ((t (:underline (:color "#B4637A" :style wave)))))
   '(flycheck-info         ((t (:underline (:color "#286983" :style wave)))))
   '(flycheck-info-face    ((t (:underline (:color "#286983" :style wave)))))
   '(flycheck-warning      ((t (:underline (:color "#E5E5E5" :style wave)))))
   '(flycheck-warning-face ((t (:underline (:color "#E5E5E5" :style wave))))))
  (setq flycheck-idle-change-delay 1.0
        flycheck-buffer-switch-check-intermediate-buffers t
        flycheck-display-errors-delay 0.25)
  :custom
  (flycheck-check-syntax-automatically '(save mode-enabled)))

(use-package company
  :config
  (custom-set-faces
   '(company-echo-common ((t (:background "#D7827E"))))
   '(company-preview ((t (:inherit (company-tooltip-selection company-tooltip)))))
   '(company-scrollbar-bg ((t (:background "#F2E9DE"))))
   '(company-scrollbar-fg ((t (:background "#D7827E"))))
   '(company-template-field ((t (:background "#f6c177" :foreground "#575279"))))
   '(company-tooltip ((t (:background "#FAF4ED" :foreground "#575279"))))
   '(company-tooltip-annotation ((t (:foreground "#D7827E"))))
   '(company-tooltip-common ((t (:foreground "#D7827E"))))
   '(company-tooltip-selection ((t (:background "#F2E9DE")))))
  (setq company-idle-delay 0.0
        company-minimum-prefix-length 2
        company-tooltip-limit 14
        company-tooltip-align-annotations t
        company-require-match 'never
        company-global-modes '(not erc-mode message-mode help-mode gud-mode)
        company-frontends
        '(company-pseudo-tooltip-frontend
          company-echo-metadata-frontend)
        company-backends '(company-capf)
        company-auto-complete nil
        company-auto-complete-chars nil
        company-dabbrev-other-buffers nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil))

;;; -- Language support (LSP)

;; LSP for c/cpp
(use-package ccls
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))     ;; requires ccls, cmake, clang (install it w/ your package manager)


;; add this to your Clojure config
;; LSP for Clojure
(add-hook 'clojure-mode-hook 'lsp)
(add-hook 'clojurescript-mode-hook 'lsp)
(add-hook 'clojurec-mode-hook 'lsp)

;; Note to install the lsp-server for Clojure you will need to do:
;;; M-x lsp-install-server RET clojure-lsp

(provide 'lsp-mode)
