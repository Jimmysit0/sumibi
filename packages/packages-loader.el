;; If you don't wanna load something, just comment it using ;;
;;; If you only wanna comment a part of a file, get into it and comment that part!

;;; Elcord --- Allows you to integrate Rich Presence from Discord
(require 'elcord)
(elcord-mode)

;;; Languages --- Everything related to the languages (plugins) :)
(require 'langs)

;;; Lsp mode and others --- Configuration for LSP mode & some extra stuff
(require 'lsp-mode)

;;; Utils --- Various tools :)
(require 'utils)

;;; Yasnippet --- This is pretty self explanatory, don't you think so?
(require 'yasnippet)

(provide 'packages-loader)
