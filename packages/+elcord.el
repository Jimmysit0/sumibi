;; Elcord -*- lexical-binding: t; -*-

(use-package elcord
  :config
  (setq elcord-display-buffers-details nil
        elcord-display-elapsed t
        elcord-show-small-icon nil
        elcord-use-major-mode-as-main-icon t
        elcord-refresh-rate 0.25))

(elcord-mode)

(provide '+elcord)
