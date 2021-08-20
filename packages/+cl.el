;; common-lisp.el -*- lexical-binding: t; -*-
                                        ;
-- SLY specifics
(use-package sly
  :hook (lisp-mode-local-vars . sly-editing-mode)
  :custom
  (inferior-lisp-program "sbcl")                  ; SBCL Rocks
  (sly-command-switch-to-existing-lisp 'always)
  (sly-ignore-protocol-mismatches t)
  (sly-kill-without-query-p t)
  (sly-mrepl-history-file-name
   (expand-file-name ".sly-mrepl-history" (expand-file-name ".cache" user-emacs-directory)))
  (sly-net-coding-system 'utf-8-unix)
  :config
  (setq sly-kill-without-query-p t
        sly-net-coding-system 'utf-8-unix
        sly-complete-symbol-function 'sly-simple-completions)

  ;; From: https://github.com/hlissner/doom-emacs/tree/develop/modules/lang/common-lisp
  (defun +common-lisp--cleanup-sly-maybe-h ()
        "Kill processes and leftover buffers when killing the last sly buffer."
        (unless (cl-loop for buf in (delq (current-buffer) (buffer-list))
                         if (and (buffer-local-value 'sly-mode buf)
                                 (get-buffer-window buf))
                         return t)
          (dolist (conn (sly--purge-connections))
            (sly-quit-lisp-internal conn 'sly-quit-sentinel t))
          (let (kill-buffer-hook kill-buffer-query-functions)
            (mapc #'kill-buffer
                  (cl-loop for buf in (delq (current-buffer) (buffer-list))
                           if (buffer-local-value 'sly-mode buf)
                           collect buf)))))
  (add-hook 'sly-mode-hook
            (lambda ()
              (unless (sly-connected-p)
                (save-excursion (sly)))
              (add-hook 'kill-buffer-hook #'+common-lisp--cleanup-sly-maybe-h nil t)))

  (defun kill-sly-buffers ()
    (interactive)
    (dolist (buffer (buffer-list))
      (when (or (eql (string-match "\\*sly" (buffer-name buffer)) 0)
                (eql (string-match " \\*sly" (buffer-name buffer)) 0))
        (kill-buffer buffer)))
    t)

  (defun kill-sly-buffers-on-close (process)
    (my/kill-sly-buffers)))

(eval-after-load 'sly
  `(define-key sly-prefix-map (kbd "M-h") 'sly-documentation-lookup))

(use-package sly-asdf
  :after (sly)
  :bind (:map sly-mode-map
              ("C-c L" . my/load-current-system-or-ask))
  :defer nil
  :config
  (defun my/load-current-system-or-ask ()
    (interactive)
    (if (sly-connected-p)
        (sly-asdf-load-system (or (sly-asdf-find-current-system) (sly-asdf-read-system-name)))
      (message "Not connected.")))

  (define-advice sly-asdf-read-system-name (:around (orig-function &optional prompt default-value)
                                                    my/sly-asdf-read-system-name-prefer-history)
    (funcall orig-function prompt (or default-value (car sly-asdf-system-history) (sly-asdf-find-current-system)))))

(use-package sly-macrostep
  :after (sly))

(use-package sly-named-readtables
  :after (sly))

(use-package sly-repl-ansi-color                  ; Colors in the SLY REPL
  :defer t
  :init
  (add-to-list 'sly-contribs 'sly-repl-ansi-color))

(provide '+cl)
