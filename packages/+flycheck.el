;; Flycheck -*- lexical-binding: t; -*-

(use-package flycheck
  :hook (prog-mode . global-flycheck-mode)
  :config
  (setq flycheck-global-modes '(not LaTeX-mode latex-mode lisp-mode emacs-lisp-mode))) 

(provide '+flycheck)
