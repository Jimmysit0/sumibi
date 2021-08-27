;; screenshot -*- lexical-binding: t; -*-

(straight-use-package
 '(screenshot
  :host github
  :repo "tecosaur/screenshot"
  :branch "master"))

(use-package transient)
(use-package posframe)

(provide '+screenshot)
