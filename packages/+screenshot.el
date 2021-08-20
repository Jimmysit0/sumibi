;; c/c++ -*- lexical-binding: t; -*-

;; git goodies

(straight-use-package
 '(screenshot
  :host github
  :repo "tecosaur/screenshot"
  :branch "master"))

(use-package transient)
(use-package posframe)

(provide '+screenshot)
