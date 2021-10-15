;; company.el -*- lexical-binding: t; -*-

(use-package company
  :hook
  (prog-mode . company-mode)
  :custom
  (company-idle-delay 0.2)
  (company-tooltip-limit 14)
  (company-tooltip-align-annotations t)
  (company-minimum-prefix-length 1)
  (company-selection-wrap-around t)
  (company-backends '(company-capf))
  (company-format-margin-function nil))

(use-package prescient
  :defer 1
  :config
  (prescient-persist-mode 1))

(provide '+company)
