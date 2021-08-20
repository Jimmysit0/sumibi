;;; mnml command interface -*- lexical-binding: t; -*-

(define-minor-mode sumibi-command-mode
  "sumibi command mode"
  :keymap  (make-sparse-keymap))

(defface sumibi-face-command nil
  "Face for the whole header line command"
  :group 'sumibi)

(defface sumibi-face-command-prompt nil
  "Face for header line command prompt"
  :group 'sumibi)

(defface sumibi-face-command-cursor nil
  "Face for header line command cursor"
  :group 'sumibi)

(set-face-attribute 'sumibi-face-command nil
                    :foreground sumibi-color-foreground
                    :background sumibi-color-subtle
                    :box `(:line-width 1
                           :color ,sumibi-color-foreground
                           :style nil)
                   :inherit nil)

(set-face-attribute 'sumibi-face-command-prompt nil
                    :inherit 'sumibi-face-strong
                    :foreground sumibi-color-background
                    :background sumibi-color-foreground
                    :box `(:line-width 1
                           :color ,sumibi-color-foreground
                           :style nil))

(set-face-attribute 'sumibi-face-command-cursor nil
                    :foreground sumibi-color-background
                    :background sumibi-color-foreground)

(defvar sumibi-command--slave nil
  "Slave buffer displaying the command.")

(defvar sumibi-command--master "*sumibi-command*"
  "Master buffer recording keystrokes.")

(defvar sumibi-command--cookie nil
  "Cookie returned by face-remap-add-relative.")

(defun sumibi-command--update ()
  "This function makes sure the content of the master buffer is copied
to the slave buffer header line and cursor stays on first line."

  ;; Makes sure cursor stays on first line
  (with-current-buffer sumibi-command--master
   (let ((eol (save-excursion (goto-char (point-min)) (point-at-eol))))
    (if (> (point) eol) (goto-char eol))))

  ;; Update slave header line
  (with-current-buffer sumibi-command--slave
    (force-mode-line-update nil)))


(defun sumibi-command--check-focus (&rest args)
  "This function check if the maste buffer has focus.
If not, it closes sumibi command."

  (if (not (eq (selected-window)
               (get-buffer-window sumibi-command--master)))
      (sumibi-command--close)))

(defun sumibi-command--close ()
  "Close sumibi command"

  (interactive)

  ;; Remove advice
  (advice-remove #'select-window #'sumibi-command--check-focus)

  ;; Close master window
  (when (window-live-p (get-buffer-window sumibi-command--master))
    (delete-window (get-buffer-window sumibi-command--master)))

  ;; Kill master buffer
  (when (get-buffer sumibi-command--master)
    (kill-buffer sumibi-command--master))

  ;; Restore slave to normal state
  (with-selected-window (get-buffer-window sumibi-command--slave)
    (kill-local-variable 'header-line-format)
    (face-remap-remove-relative sumibi-command--cookie))

  ;; Update mode lines
  (force-mode-line-update t))


(defun sumibi-command (&optional prompt callback content information)

  ;; Cannot open sumibi command while in minibuffer
  (when (minibufferp)
    (error "Cannot open sumibi command while in minibuffer"))

  ;; Cannot open sumibi command while in sumibi command
  (when (eq (current-buffer) (get-buffer sumibi-command--master))
    (error "Cannot open sumibi command while in mini command"))

  ;; Kill the master buffer & window if openened (not strictly necessary)
  (when (window-live-p (get-buffer-window sumibi-command--master))
    (delete-window (get-buffer-window sumibi-command--master))
    (kill-buffer sumibi-command--master))

  ;; Save the slave buffer
  (setq sumibi-command--slave (current-buffer))

  ;; Install sumibi face command in the slave buffer
  (setq sumibi-command--cookie
        (face-remap-add-relative 'header-line 'sumibi-face-command))

  ;; Create master buffer by splitting slave buffer
  (let ((window-min-height 1)
        (window-safe-min-height 1)
        (window-resize-pixelwise t)
        (split-window-keep-point t))
    (with-selected-window (split-window-vertically -2)
      (switch-to-buffer (get-buffer-create sumibi-command--master))
      (erase-buffer)
      (org-mode)
      (sumibi-command-mode)
      (if content (insert content))
      (insert "\n")
      (insert "-")

      ;; This tries to hide most of the master window
      (goto-char (point-min))
      (overlay-put (make-overlay (point-at-bol) (+ (point-at-eol) 1))
                   'face '(:height 10))
     (setq cursor-type nil)

      (setq header-line-format nil)
      (setq mode-line-format nil)
      (face-remap-add-relative 'default `(:foreground ,sumibi-color-background))
      (face-remap-add-relative 'region  `(:background ,sumibi-color-background))
      (fit-window-to-buffer)
      (setq window-size-fixed t)

      ;; History
      ;; (goto-char (point-max))
      ;; (insert "history-item-1 history-item-2 history-item-3")
      ;; (goto-char (point-min))
      ))


  ;; Install header line in master buffer
  (setq header-line-format
        (list

         ;; Prompt + one space
         (propertize " "  'face 'sumibi-face-command-prompt
		          'display `(raise -0.20))
         (propertize (or prompt "M-x") 'face 'sumibi-face-command-prompt)
         (propertize " "  'face 'sumibi-face-command-prompt
	  	          'display `(raise +0.15))
         (propertize " " )

         ;; Input (copied from master). we need to add a space at end
         ;; of content to be able to show cursor when it is at the end
         ;; of the line.
         `(:eval
           (let* ((content (with-current-buffer sumibi-command--master
                             (save-excursion (goto-char (point-min))
                               (buffer-substring (point-at-bol) (point-at-eol)))))
                  (content (cond ((> (length content) 0)
                                  (concat content " "))
                                 ((> (length ,information) 0)
                                  (propertize ,information 'face 'sumibi-face-faded))
                                 (t " ")))
                  (point  (with-current-buffer sumibi-command--master (point)))
                  (region (with-current-buffer sumibi-command--master (region-active-p))))

             ;; Cursor
             (put-text-property (- point 1) point
                                'face 'sumibi-face-command-cursor content)
             ;; Region
             (if region
                 (let ((beg (with-current-buffer sumibi-command--master (region-beginning)))
                       (end (with-current-buffer sumibi-command--master (region-end))))
                   (put-text-property (- beg 1) (- end 1)
                                      'face `(:foreground ,sumibi-color-background
                                              :background ,sumibi-color-faded) content)))
             content))))

  ;; Install key bindings
  (with-current-buffer sumibi-command--master
    (add-hook 'post-command-hook 'sumibi-command--update nil t)
    (define-key sumibi-command-mode-map (kbd "C-g") #'sumibi-command--close)
    (define-key sumibi-command-mode-map (kbd "<tab>")
      #'(lambda() (interactive) (dabbrev-expand nil)))

    (define-key sumibi-command-mode-map (kbd "<return>")
      (lambda ()
        (interactive)
        (let* ((content (with-current-buffer sumibi-command--master
                          (save-excursion (goto-char (point-min))
                          (buffer-substring (point-at-bol) (point-at-eol))))))
          (sumibi-command--close)
          (if callback (funcall callback content)
            (message content))))))

  ;; Update mode lines and swicch to master buffer
  (sumibi-command--update)
  (select-window (get-buffer-window sumibi-command--master) t)
  (redisplay)

  ;; Advice after select window to check for focus
  (advice-add #'select-window :after #'sumibi-command--check-focus))


(defun sumibi-command-x ()
  (interactive)
  (sumibi-command "M-x"  #'sumibi-command-x-finalize "" "Enter command"))

(defun sumibi-command-x-finalize (command)
  (interactive)
  (command-execute (intern command)))


(defun sumibi-command-shell ()
  (interactive)
  (sumibi-command ">_"  #'sumibi-command-shell-finalize
                "" "Enter shell command"))

(defun sumibi-command-shell-finalize (command)
  (interactive)
  (shell-command command)
  (switch-to-buffer "*Shell Command Output*"))

(define-key global-map (kbd "M-x") #'sumibi-command-x)
(define-key global-map (kbd "M-s") #'sumibi-command-shell)

(provide 'sumibi-command)
