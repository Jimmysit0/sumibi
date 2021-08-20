;; c/c++ -*- lexical-binding: t; -*-

;; git goodies

(use-package elescope
  :config
  (setf elescope-root-folder (concat (getenv "$HOME") "/opt"))
  (setf elescope-clone-depth nil
        elescope-use-full-path t))

(provide '+git)
