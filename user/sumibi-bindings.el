;;; bindings -*- lexical-binding: t; -*-

;; Kill current buffer (instead of asking first buffer name)
(global-set-key (kbd "C-x k") 'kill-current-buffer)

;; M-n for new frame (M-n is unbound in vanilla emacs)
(defun new-frame ()
  (interactive)
  (select-frame (make-frame))
  (switch-to-buffer "*scratch*"))
(global-set-key (kbd "M-n") 'new-frame)
(global-set-key (kbd "M-`") 'other-frame)

;; M-return for frame maximization toggle
(global-set-key (kbd "<M-return>") 'toggle-frame-maximized)
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "<M-return>") 'toggle-frame-maximized))

;; Close frame if not the last, kill emacs else
(defun sumibi--delete-frame-or-kill-emacs ()
  "Delete frame or kill Emacs if there is only one frame."
  (interactive)
  (if (> (length (frame-list)) 1)
      (delete-frame)
    (save-buffers-kill-terminal)))
(global-set-key (kbd "C-x C-c") 'sumibi--delete-frame-or-kill-emacs)

(provide 'sumibi-bindings)

