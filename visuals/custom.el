;;; custom.el -*- lexical-binding: t; -*-
(custom-set-variables
 '(geiser-guile-binary "guile2.2")
 '(flycheck-check-syntax-automatically '(save mode-enabled)))

(custom-set-faces
 '(ctrlf-highlight-active ((t :inherit 'sumibi-face-strong)))
 '(ctrlf-highlight-line ((t :inherit 'sumibi-face-background)))
 '(rainbow-delimiters-base-error-face ((t (:foreground "#E0A1B0 " :background "#F8ECEF"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#5C6A6C"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#BDC3C4"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#E0A1B0"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#BDC3C4"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#C0AEC6"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#BDC3C4"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#9CA4A5"))))
 '(rainbow-delimiters-max-face-count 8)
 '(flycheck-error        ((t (:underline (:color "#CD6780" :style wave)))))
 '(flycheck-info         ((t (:underline (:color "#9FC2DD" :style wave)))))
 '(flycheck-info-face    ((t (:underline (:color "#9FC2DD" :style wave)))))
 '(flycheck-warning      ((t (:underline (:color "#EA9D34" :style wave)))))
 '(flycheck-warning-face ((t (:underline (:color "#EA9D34" :style wave))))))

(require 'paren)
(set-face-background 'show-paren-match (face-background 'default))
(set-face-foreground 'show-paren-match "#def")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)
