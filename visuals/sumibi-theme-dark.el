;;; dark-theme -*- lexical-binding: t; -*-

(require 'sumibi-base-colors)

(defun sumibi-theme-set-dark ()
  (setq frame-background-mode    'dark)
  (setq sumibi-color-foreground "#fdf4c1")
  (setq sumibi-color-strong     "#fdf4c1")
  (setq sumibi-color-background "#1d2021")
  (setq sumibi-color-highlight  "#1d2021")
  (setq sumibi-color-critical   "#fb4933")
  (setq sumibi-color-salient    "#fb4933")
  (setq sumibi-color-popout     "#b8bb26")
  (setq sumibi-color-subtle     "#333637")
  (setq sumibi-color-faded      "#777979")
  (setq sumibi-color-region     "#333637")
  
  ;; programming
  (setq sumibi-color-numbers    "#f0f1d3")
  (setq sumibi-color-function   "#211e1d")
  (setq sumibi-color-comment    "#17191a"))



(sumibi-theme-set-dark)

(provide 'sumibi-theme-dark)
