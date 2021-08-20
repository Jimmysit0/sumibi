;; Parinfer -*- lexical-binding: t; -*-

(use-package parinfer-rust-mode
  :hook (prog-mode . parinfer-rust-mode)
  :config
  (setq parinfer-rust-auto-download t)
  (electric-indent-mode +1))    

(provide '+parinfer)
