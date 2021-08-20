;;; light-theme -*- lexical-binding: t; -*-

(require 'sumibi-base-colors)
(defun sumibi-theme-set-light ()
  (setq frame-background-mode    'light)
  (setq sumibi-color-foreground "#5C6A6C")
  (setq sumibi-color-strong     "#5C6A6C")
  (setq sumibi-color-background "#FFFFFF")
  (setq sumibi-color-highlight  "#FAF9FB")
  (setq sumibi-color-critical   "#E0A1B0")
  (setq sumibi-color-salient    "#C0AEC6")
  (setq sumibi-color-popout     "#C0AEC6")
  (setq sumibi-color-subtle     "#FAF9FB")
  (setq sumibi-color-faded      "#9CA4A5"))
(sumibi-theme-set-light)

(provide 'sumibi-theme-light)
