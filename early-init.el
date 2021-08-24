;;; early-init.el -*- lexical-binding: t; -*-

;;; Garbage Collector
(setq gc-cons-threshold most-positive-fixnum
          gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
  (lambda ()
        (setq gc-cons-threshold 100000000
                  gc-cons-percentage 0.1)))

(setq read-process-output-max (* 1024 1024))

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. But sumibi always load it!
(setq package-enable-at-startup nil)

(setq tool-bar-mode nil
      menu-bar-mode nil)
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

