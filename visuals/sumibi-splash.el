;; sumibi-layout.el
;;
;; This file is the start screen you see when you just open sumibi-emacs
;;
;;; Code:
(require 'subr-x)
(require 'cl-lib)

(defun sumibi-splash ()
  "sumibi Emacs splash screen"

  (interactive)

  ;; Hide modeline before window-body-height is computed
  (let* ((splash-buffer (get-buffer-create "*splash*")))
        (with-current-buffer splash-buffer
          (setq header-line-format nil)
          (setq mode-line-format nil)))

  (let* ((splash-buffer  (get-buffer-create "*splash*"))
                 (height         (round (- (window-body-height nil) 1) ))
                 (width          (round (window-body-width nil)        ))
                 (padding-center (+ (/ height 2) 1)))

        ;; If there are buffer associated with filenames,
        ;;  we don't show the splash screen.
        (if (eq 0 (length (cl-loop for buf in (buffer-list)
                                                        if (buffer-file-name buf)
                                                        collect (buffer-file-name buf))))

                (with-current-buffer splash-buffer
                  (erase-buffer)

                  ;; Buffer local settings
                  (if (one-window-p) (setq mode-line-format nil))
                  (setq cursor-type nil)
                  (setq line-spacing 0)
                  (setq vertical-scroll-bar nil)
                  (setq horizontal-scroll-bar nil)
                  (setq fill-column width)
                  (face-remap-add-relative 'link :underline nil)
                  (if (not (display-graphic-p)) (menu-bar-mode 0))

                  ;; Top text : )
                  (insert (propertize (format-message "Init: %.2f seconds"
                  (float-time (time-subtract after-init-time before-init-time)))
                  'face 'sumibi-face-faded))
                  (center-line)
                  (insert "\n")

                  (insert-char ?\n padding-center)  ;; Vertical padding to center
                  (insert (propertize "Welcome to sumibi! Type M-h for help" 'face 'sumibi-face-faded))
                  (center-line)
                  (insert "\n")

                  (goto-char 0)
                  (read-only-mode t)
                  (local-set-key [t] 'sumibi-splash-kill)
                  (display-buffer-same-window splash-buffer nil)
                  (run-with-idle-timer 0.05 nil (lambda() (message nil)))
                  (run-with-idle-timer 0.50 nil 'sumibi-splash-fade-out-slow)
                  (run-with-idle-timer 0.55 nil 'sumibi-splash-help-message)))))


(defun center-string (string)
  "Pad a string with space on the left such as to center it"
  (let* ((padding (/ (- (window-body-width) (length string)) 2))
                 (padding (+ (length string) padding)))
        ;; If the string is displayed as a tooltip, don't pad it
        (if (and tooltip-mode (fboundp 'x-show-tip))
                string
          (format (format "%%%ds" padding) string))))

(defun sumibi-splash-kill ()
  "Kill the splash screen buffer (immediately)."
  (interactive)
  (if (get-buffer "*splash*")
          (progn (message nil)
                         (cancel-function-timers 'sumibi-splash-fade-out-slow)
                         (cancel-function-timers 'sumibi-spash-help-message)
                         (kill-buffer "*splash*"))))

(defun sumibi-splash-help-message ()
  (message "Logged in as %s, don't forget to have fun!" user-full-name))


;; Install hook after frame parameters have been applied and only if
;; no option on the command line
(if (and (not (member "-no-splash"  command-line-args))
                 (not (member "--file"      command-line-args))
                 (not (member "--insert"    command-line-args))
                 (not (member "--find-file" command-line-args))
                 ;; (not inhibit-startup-screen)
                 )
        (progn
          (add-hook 'window-setup-hook 'sumibi-splash)
          (setq inhibit-startup-screen t
                        inhibit-startup-message t
                        inhibit-startup-echo-area-message t)))


(provide 'sumibi-splash)
