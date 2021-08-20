;; load-theme -*- lexical-binding: t; -*-

(require 'sumibi-layout)

(setq sumibi-font-family-monospaced "Comic Code"
          sumibi-font-size 13)

(add-to-list 'command-switch-alist '("-light"  . (lambda (args))))
(add-to-list 'command-switch-alist '("-default"  . (lambda (args))))

(cond
 ((member "-default" command-line-args) t)
 (t (require 'sumibi-theme-light)))

(require 'sumibi-faces)
(sumibi-faces)

(require 'sumibi-theme)
(sumibi-theme)

(require 'sumibi-modeline)
(require 'sumibi-bindings)
(require 'sumibi-defaults)
(require 'sumibi-command)
(require 'sumibi-writer)

(provide '+sumibi-theme)
