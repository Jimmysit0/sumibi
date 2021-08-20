;; c/c++ -*- lexical-binding: t; -*-

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

(provide '+c)
