;; visuals.el -*- lexical-binding: t; -*-

(straight-use-package
 '(ct
   :host github
   :repo "neeasade/ct.el"
   :branch "master"))

(use-package rainbow-mode)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(straight-use-package
 '(ws-butler
   :hook (after-init . ws-butler-global-mode)
   :host github
   :repo "hlissner/ws-butler"
   :branch "master"
   :config
   (setq ws-butler-keep-whitespace-before-point nil)))

(provide '+parentheses)
