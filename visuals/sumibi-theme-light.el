;;; -*- lexical-binding: t; -*-

(require 'sumibi-base-colors)

(defun sumibi-theme-set-light ()
  (setq frame-background-mode    'light)
  
  ;; basic colors
  (setq sumibi-color-background "#f4f4f4")
  (setq sumibi-color-foreground "#161616")
  (setq sumibi-color-highlight  "#f4f4f4")
  (setq sumibi-color-strong     "#525252")
  (setq sumibi-color-critical   "#da1e28")
  (setq sumibi-color-salient    "#0f62fe")
  (setq sumibi-color-popout     "#0f62fe")
  (setq sumibi-color-subtle     "#e0e0e0")
  (setq sumibi-color-faded      "#8d8d8d")
  (setq sumibi-color-region     "#ffd6e8")
  
  ;; programming
  (setq sumibi-color-numbers    "#491d8b")
  (setq sumibi-color-function   "#d0e2ff")
  (setq sumibi-color-comment    "#fdf0ca"))

(sumibi-theme-set-light)

(provide 'sumibi-theme-light)

;;; sumibi-theme-light.el ends here
