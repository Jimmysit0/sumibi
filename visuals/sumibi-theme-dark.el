;;; dark-theme -*- lexical-binding: t; -*-

(require 'sumibi-base-colors)

(defun sumibi-theme-set-dark ()
  (setq frame-background-mode    'dark)

  ;; basic colors
  (setq sumibi-color-foreground "#d3c6aa")
  (setq sumibi-color-strong     "#d3c6aa")
  (setq sumibi-color-background "#2f383e")
  (setq sumibi-color-highlight  "#374247")
  (setq sumibi-color-critical   "#d7b97d")
  (setq sumibi-color-salient    "#a7c080")
  (setq sumibi-color-popout     "#a7c080")
  (setq sumibi-color-subtle     "#868d80")
  (setq sumibi-color-faded      "#374247")
  (setq sumibi-color-region     "#404c51")

  ;; programming
  (setq sumibi-color-numbers    "#d699b6")
  (setq sumibi-color-function   "#374247")
  (setq sumibi-color-comment    "#374247"))

(sumibi-theme-set-dark)

(provide 'sumibi-theme-dark)

;;; sumibi-theme-dark.el ends here
