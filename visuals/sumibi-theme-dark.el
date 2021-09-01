;;; dark-theme -*- lexical-binding: t; -*-

(require 'sumibi-base-colors)

(defun sumibi-theme-set-dark ()
  (setq frame-background-mode    'dark)
  (setq sumibi-color-foreground "#ECEFF4")
  (setq sumibi-color-strong     "#ECEFF4")
  (setq sumibi-color-background "#131a21")
  (setq sumibi-color-highlight  "#434C5E")
  (setq sumibi-color-critical   "#f9929b")
  (setq sumibi-color-salient    "#d7c1ed")
  (setq sumibi-color-popout     "#7ed491")
  (setq sumibi-color-subtle     "#434C5E")
  (setq sumibi-color-faded      "#bac8ef"))

(sumibi-theme-set-dark)

(provide 'sumibi-theme-dark)
