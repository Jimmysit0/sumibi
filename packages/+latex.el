;; -- Latex -*- lexical-binding: t; -*-

(use-package auctex
  :defer t
  :ensure auctex
  :config
  (setq TeX-auto-save t))

;; Utils for LaTeX
(use-package adaptive-wrap)
(use-package latex-preview-pane)

(defun sumibi/toggle-latex-preview-pane-mode ()
  "Toggle the panel when using LaTeX"
  (interactive)
  (latex-preview-pane-mode 'toggle))

(global-set-key (kbd "C-q") 'sumibi/toggle-latex-preview-pane-mode)

(setq doc-view-continuous t)                      ; To have continuous scrolling in the preview-pane

(provide '+latex)
