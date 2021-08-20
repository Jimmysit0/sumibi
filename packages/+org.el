;; Org mode -*- lexical-binding: t; -*-

(use-package org
  :init
  (setq org-catch-invisible-edits 'smart
        org-ellipsis "..."
        org-indent-indentation-per-level 1
        org-special-ctrl-a/e t
        org-src-preserve-indentation t
        org-src-tab-acts-natively t
        org-startup-indented t
        org-support-shift-select t
        org-adapt-indentation nil)
  :hook
  (org-mode . visual-line-mode)
  :config
  (setq org-directory "~/opt/notes")
  (define-key org-mode-map "\C-a" 'org-beginning-of-line)
  (define-key org-mode-map "\C-e" 'org-end-of-line))

;; org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)
   (clojure . t)
   (css . t)
   (emacs-lisp . t)
   (js . t)
   (lisp . t)))

(push '("conf-unix" . conf-unix) org-src-lang-modes)

(provide '+org)
