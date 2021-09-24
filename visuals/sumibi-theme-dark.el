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
  (setq sumibi-color-subtle     "#434C5E")
  (setq sumibi-color-faded      "#7c6f64"))

(sumibi-theme-set-dark)

(provide 'sumibi-theme-dark)
