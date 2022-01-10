;; Elcord -*- lexical-binding: t; -*-

(defun sumibi/elcord-buffer-details-format ()
  "Return the buffer details string shown on discord."
  (format "%s" (buffer-name)))

(use-package elcord
  :config
  (setq elcord-mode-icon-alist '((dashboard-mode . "elisp-mode_icon")
				                 (fundamental-mode . "elisp-mode_icon")
				                 (c-mode . "c-mode_icon")
				                 (c++-mode . "c_-mode_icon")
                                                      (crystal-mode . "crystal-mode_icon")
				                 (clojure-mode . "clojure-mode_icon")
                                                      (css-mode . "css-mode_icon")
				                 (dired-mode . "elisp-mode_icon")
				                 (emacs-lisp-mode . "elisp-mode_icon")
				                 (eshell-mode . "elisp-mode_icon")
				                 (haskell-mode . "haskell-mode_icon")
                                                      (haxe-mode . "haxe-mode_icon")
				                 (haskell-interactive-mode . "haskell-mode_icon")
				                 (js-mode . "javascript-mode_icon")
				                 (magit-mode . "magit-mode_icon")
				                 (markdown-mode . "markdown-mode_icon")
                                                      (nixos-mode . "nixos-mode_icon")
                                                      (latex-mode . "latex-mode_icon")
                                                      (text-mode . "elisp-mode_icon")
                                                      (org-mode . "org-mode_icon")
				                 ("^slime-.*" . "lisp-mode_icon")
				                 ("^sly-.*$" . "lisp-mode_icon")
				                 (typescript-mode . "typescript-mode_icon")
                                                      (writer-mode . "org-mode_icon")
                                                      (term-mode . "x-mode_icon")
                                                      (shell-mode . "x-mode_icon")
				                 (vterm-mode . "x-mode_icon")))
  (setq elcord-client-id "930189119437549648")
  (setq elcord-quiet nil
        elcord-editor-icon "elisp-mode_icon"
        elcord-buffer-details-format-function 'sumibi/elcord-buffer-details-format
        elcord-display-buffer-details t
        elcord-display-elapsed nil
        elcord-show-small-icon nil
        elcord-use-major-mode-as-main-icon t
        elcord-refresh-rate 0.25))

(elcord-mode)

(provide '+elcord)
