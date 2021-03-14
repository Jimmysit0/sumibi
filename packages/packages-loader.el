;; If you don't wanna load something, just comment it using ;;
;;; If you only wanna comment a part of a file, get into it and comment that part!

;;; Elcord --- Allows you to integrate Rich Presence from Discord
(require 'elcord)
(elcord-mode)

;;; Languages --- Everything related to the languages (plugins) :)
(require 'langs)

;;; Utils --- Various tools :)
(require 'utils)

(provide 'packages-loader)
