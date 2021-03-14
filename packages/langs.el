;; langs.el

;; this is like pkgs.el, but only for langs.

;;; Code:
(require 'straight)

;; -- LaTeX
(use-package auctex
  :defer t
  :ensure auctex
  :config
  (setq TeX-auto-save t))

;; Utils for LaTeX
(use-package adaptive-wrap)
(use-package latex-preview-pane)

(defun sumibi/toggle-latex-preview-pane-mode ()
  "Toggle the panel when using LaTeX"
  (interactive)
  (latex-preview-pane-mode 'toggle))

(global-set-key (kbd "C-q") 'sumibi/toggle-latex-preview-pane-mode)

(setq doc-view-continuous t)                      ; To have continuous scrolling in the preview-pane

; -- Utils
(use-package eros)                                ; Inline evaluation
(eros-mode 1)                                     ; Initialize `eros-mode'


; --- LISPS

;; -- Clojure - a dialect of Lisp
(use-package clojure-mode)
(use-package clojure-mode-extra-font-locking)
(use-package cider)

;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))

;; -- `CIDER' specifics
(add-hook 'cider-mode-hook 'eldoc-mode)           ; Minibuffer documentation for the code you're typing into the repl
(setq cider-repl-pop-to-buffer-on-connect t)      ; Go right to the REPL buffer when it's finished connecting
(setq cider-show-error-buffer t)                  ; When there's a cider error, show its buffer and switch to it
(setq cider-auto-select-error-buffer t)
(setq cider-repl-history-file                     ; Where to store cider's history
          "~/.emacs.d/cider-history")
(setq cider-repl-wrap-history t)                  ; Wrap when navigating history.

(eval-after-load 'cider
  '(progn
         (define-key clojure-mode-map (kbd "C-c C-v") 'cider-start-http-server)
         (define-key clojure-mode-map (kbd "C-M-r")   'cider-refresh)
         (define-key clojure-mode-map (kbd "C-c u")   'cider-user-ns)
         (define-key cider-mode-map   (kbd "C-c u")   'cider-user-ns)))

(defun cider-start-http-server ()
  (interactive)
  (cider-load-current-buffer)
  (let ((ns (cider-current-ns)))
        (cider-repl-set-ns ns)
        (cider-interactive-eval (format "(println '(def server (%s/start))) (println 'server)" ns))
        (cider-interactive-eval (format "(def server (%s/start)) (println server)" ns))))

(defun cider-refresh ()
  (interactive)
  (cider-interactive-eval (format "(user/reset)")))

(defun cider-user-ns ()
  (interactive)
  (cider-repl-set-ns "user"))

;; -- Common Lisp - the "standard" Lisp

; -- SLY specifics
(use-package sly
  :hook (lisp-mode-local-vars . sly-editing-mode)
  :custom
  (inferior-lisp-program "sbcl")                  ; SBCL Rocks
  (sly-command-switch-to-existing-lisp 'always)
  (sly-ignore-protocol-mismatches t)
  (sly-kill-without-query-p t)
  (sly-mrepl-history-file-name
   (expand-file-name ".sly-mrepl-history" (expand-file-name ".cache" user-emacs-directory)))
  (sly-net-coding-system 'utf-8-unix)
  :config
  (setq sly-kill-without-query-p t
        sly-net-coding-system 'utf-8-unix
        sly-complete-symbol-function 'sly-simple-completions)

  ;; From: https://github.com/hlissner/doom-emacs/tree/develop/modules/lang/common-lisp
  (defun +common-lisp--cleanup-sly-maybe-h ()
        "Kill processes and leftover buffers when killing the last sly buffer."
        (unless (cl-loop for buf in (delq (current-buffer) (buffer-list))
                         if (and (buffer-local-value 'sly-mode buf)
                                 (get-buffer-window buf))
                         return t)
          (dolist (conn (sly--purge-connections))
            (sly-quit-lisp-internal conn 'sly-quit-sentinel t))
          (let (kill-buffer-hook kill-buffer-query-functions)
            (mapc #'kill-buffer
                  (cl-loop for buf in (delq (current-buffer) (buffer-list))
                           if (buffer-local-value 'sly-mode buf)
                           collect buf)))))
  (add-hook 'sly-mode-hook
            (lambda ()
              (unless (sly-connected-p)
                (save-excursion (sly)))
              (add-hook 'kill-buffer-hook #'+common-lisp--cleanup-sly-maybe-h nil t)))

  (defun kill-sly-buffers ()
    (interactive)
    (dolist (buffer (buffer-list))
      (when (or (eql (string-match "\\*sly" (buffer-name buffer)) 0)
                (eql (string-match " \\*sly" (buffer-name buffer)) 0))
        (kill-buffer buffer)))
    t)

  (defun kill-sly-buffers-on-close (process)
    (my/kill-sly-buffers)))

(eval-after-load 'sly
  `(define-key sly-prefix-map (kbd "M-h") 'sly-documentation-lookup))

(use-package sly-asdf
  :after (sly)
  :bind (:map sly-mode-map
              ("C-c L" . my/load-current-system-or-ask))
  :defer nil
  :config
  (defun my/load-current-system-or-ask ()
    (interactive)
    (if (sly-connected-p)
        (sly-asdf-load-system (or (sly-asdf-find-current-system) (sly-asdf-read-system-name)))
      (message "Not connected.")))

  (define-advice sly-asdf-read-system-name (:around (orig-function &optional prompt default-value)
                                                    my/sly-asdf-read-system-name-prefer-history)
    (funcall orig-function prompt (or default-value (car sly-asdf-system-history) (sly-asdf-find-current-system)))))

(use-package sly-macrostep
  :after (sly))

(use-package sly-named-readtables
  :after (sly))

(use-package sly-repl-ansi-color                  ; Colors in the SLY REPL
  :defer t
  :init
  (add-to-list 'sly-contribs 'sly-repl-ansi-color))

;; -- Elisp - the language of the machine
;; Enhancers
(use-package a)       ; assoc lists
(use-package async)   ; async
(use-package cl-lib)  ; goodies from CL
(use-package dash)    ; list
(use-package f)       ; file
(use-package fn)      ; function
(use-package ht)      ; hash table
(use-package pcre2el) ; sane regex
(use-package s)       ; string
(use-package ts)      ; timestamps

; --- WEB
;; -- `html'
(use-package emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)            ; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode)            ; Enable Emmet's css abbreviation.
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indent-after-insert nil)))

(use-package haml-mode)
(use-package pug-mode)

(defun pug-compile-saved-file()                   ; Requires `pug-cli' from NPM
  (when (and (stringp buffer-file-name)
             (string-match "\\.pug\\'" buffer-file-name))
     (pug-compile)))
(add-hook 'after-save-hook 'pug-compile-saved-file)

(use-package slim-mode)

;; -- `css'
(use-package css-mode)
(use-package less-css-mode)
(use-package sass-mode)

;; -- `js'
(use-package add-node-modules-path)
(use-package coffee-mode)
(use-package js2-mode)
(use-package js2-refactor)
(use-package npm-mode)
(use-package rjsx-mode)

(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)
(add-hook 'js2-mode-hook 'skewer-mode)
(use-package skewer-mode)

(use-package tide)
(use-package xref-js2)

; --- elixir ----
(use-package elixir-mode)
(use-package exunit)

(use-package alchemist
  :hook (elixir-mode . alchemist-mode)
  :config
  (set-lookup-handlers! 'alchemist-mode
    :definition #'alchemist-goto-definition-at-point
    :documentation #'alchemist-help-search-at-point)
  (set-eval-handler! 'alchemist-mode #'alchemist-eval-region)
  (set-repl-handler! 'alchemist-mode #'alchemist-iex-project-run))

; --- C and C++
(straight-use-package
 '(cmake-mode
   :host github
   :repo "emacsmirror/cmake-mode"
   :files (:defaults "*")))

(use-package cuda-mode)
(use-package demangle-mode)
(use-package disaster)
(use-package glsl-mode)
(use-package modern-cpp-font-lock)
(use-package opencl-mode)

(provide 'langs)

;; org-mode
(use-package org
  :init
  (setq org-catch-invisible-edits 'smart
        org-ellipsis "⤵"
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
  (setq org-directory "~/Documents/Org")
  (define-key org-mode-map "\C-a" 'org-beginning-of-line)
  (define-key org-mode-map "\C-e" 'org-end-of-line)
  )

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
