;;; light-theme -*- lexical-binding: t; -*-

(require 'sumibi-base-colors)
(defun sumibi-theme-set-dark ()
  (setq frame-background-mode    'dark)
  (setq sumibi-color-foreground "#575279")
  (setq sumibi-color-strong     "#000000")
  (setq sumibi-color-background "#faf4ed")
  (setq sumibi-color-highlight  "#faf4ed")
  (setq sumibi-color-critical   "#b4637a")
  (setq sumibi-color-salient    "#907aa9")
  (setq sumibi-color-popout     "#d7827e")
  (setq sumibi-color-subtle     "#fffaf3")
  (setq sumibi-color-faded      "#9893a5"))
(sumibi-theme-set-dark)

(provide 'sumibi-theme-dark)
