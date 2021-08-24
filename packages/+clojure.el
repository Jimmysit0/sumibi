;;; clojure.el -*- lexical-binding: t; -*-

;; -- Clojure - a dialect of Lisp
(use-package flycheck-clj-kondo)
(use-package clojure-mode
  :config
  (require 'flycheck-clj-kondo))
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
(setq cider-show-error-buffer nil)                  ; When there's a cider error, show its buffer and switch to it
(setq cider-repl-display-help-banner nil)

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

(provide '+clojure)
