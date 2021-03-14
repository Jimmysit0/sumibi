;; Assume :defer (quicker startup times)
(setq use-package-always-defer t)

;;; PACKAGE MANAGER `straight.el'
;; -- Variables
(setq straight-use-package-by-default t
      straight-vc-git-default-clone-depth 1
      straight-cache-autoloads t)

;; -- Bootstrap `straight.el'
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; -- Enable `use-package'
(straight-use-package 'use-package)
(require 'use-package-ensure)                     ; `ensure' `:t' everything
(setq use-package-always-ensure t)

(provide 'straight-setup)
