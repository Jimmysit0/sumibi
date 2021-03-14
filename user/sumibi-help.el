;; This file defines the help message and bind it to M-h
;; [C-x C-f] Open  [⌘-w] Copy   [C-w] Cut   [C-s] Search  [C-g]   Cancel
;; [C-x C-s] Save  [C-y] Paste  [C-/] Undo  [⌘-x] Command [C-x C-c] Quit
;;
;; Usage:
;;
;;  (require 'sumibi-help)
;;  M-: (sumibi-help) or M-h
;;
;; ---------------------------------------------------------------------

(defun sumibi-help ()
  (interactive)
  (let ((message-log-max nil))
	(message
	 (concat
	  " [C-x C-f] Open  [M-w] Copy   [C-w] Cut   [C-s] Search           "
	  (propertize "[C-g]   Cancel" 'face 'bold)
	  "\n"
	  " [C-x C-s] Save  [C-y] Paste  [C-/] Undo  [⌘-x] Command          "
	  (propertize "[C-x C-c] Quit" 'face 'bold)))
	(sit-for 30)))

(setq mac-pass-command-to-system nil)
(global-set-key (kbd "M-h") 'sumibi-help)

(provide 'sumibi-help)
