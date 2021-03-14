;; -*- lexical-binding: t; -*-
;; init.el -- Where all begins

;;; Load early-init.el!
(unless (boundp 'early-init-file)
  ...)

;;; Load folders!
(add-to-list 'load-path "~/.emacs.d/core")
(add-to-list 'load-path "~/.emacs.d/user")
(add-to-list 'load-path "~/.emacs.d/visuals")
(add-to-list 'load-path "~/.emacs.d/packages")

;;; Load files!
(require 'core-loader)
(require 'user-loader)
(require 'visuals-loader)
(require 'packages-loader)

(provide 'sumibi)
