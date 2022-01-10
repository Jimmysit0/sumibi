;; sumibi-writer -*- lexical-binding: t; -*-

(require 'org)
(require 'sumibi-base-colors)
(require 'sumibi-faces)

;; 
;;             prefix  
;;           |<------>|

;; border -> |<------>| * Headline level 1  # Unnumbered
;;           |<------>| 1 Headline level 1  # Numbered
;;
;;           |<----->| ** Headline level 2  # Unnumbered
;;           |<---->| 1.1 Headline level 2  # Numbered
;;
;;           |<---->| *** Headline level 3  # Unumbered
;;           |<-->| 1.1.1 Headline level 3  # Numbered
;; etc.
;;
;; This works if the number of sections at a given level is < 10.

(defun writer-mode--num-format (numbering)
  "Alternative numbering format for org-num.
First level: 1 | xxx
Second level: 1.1 — xxx
Third level: 1.1.1 - xxx
etc.
"""
  (if (= (length numbering) 1)
      (propertize (concat (mapconcat
                           #'number-to-string
                           numbering ".") " | " )
                  'face `(:family "Codelia"
                          :height 250
                          :foreground ,sumibi-color-faded))
    (propertize (concat (mapconcat
                         #'number-to-string
                         numbering ".") " — " )
                'face `(:family "Comic Code"
                        :foreground ,sumibi-color-faded))))

;; Specific face for headline stars
(font-lock-add-keywords 'writer-mode
             '(("^*+ " 0 `(:family "Codelia"
                           :height 140
                           :foreground ,sumibi-color-faded) prepend)
               ) 'append)

(defun writer-mode--compute-prefixes ()
  "Compute prefix strings for regular text and headlines."

  (setq org-indent--heading-line-prefixes
        (make-vector org-indent--deepest-level nil))
  (setq org-indent--inlinetask-line-prefixes
        (make-vector org-indent--deepest-level nil))
  (setq org-indent--text-line-prefixes
        (make-vector org-indent--deepest-level nil))
        
  (let* ((min-indent 5)
         (indent (+ 1 (seq-max 
                  (org-element-map
                      (org-element-parse-buffer) 'headline
                    #'(lambda (item)
                        (org-element-property :level item))))))
         (indent (max indent min-indent)))
    
  (dotimes (n org-indent--deepest-level)
    (aset org-indent--heading-line-prefixes n
          (make-string
           (min indent (max 0 (- indent 1 n))) ?\s))
    (aset org-indent--inlinetask-line-prefixes n
          (make-string indent ?\s))
    (aset org-indent--text-line-prefixes n
          (make-string indent ?\s)))))



(define-derived-mode writer-mode org-mode "sumibi writer"

  ;; Faces
  (face-remap-add-relative 'org-level-1
                           :overline sumibi-color-subtle
                           :family "Comic Code" :height 180)
  (face-remap-add-relative 'org-level-2
                           :family "Comic Code" :height 160)
  (face-remap-add-relative 'org-level-3
                           :family "Comic Code" :height 150)
  (face-remap-add-relative 'org-document-info
                           :inherit 'sumibi-face-faded)
  (face-remap-add-relative 'org-document-title
                           :foreground sumibi-color-foreground
                           :family "Captain Comic" 
                           :height 200
                           :weight 'medium)
  ;; hide title / author ... keywords
  (setq-local org-hidden-keywords '(title author date startup))

  ;; Header line
  (setq header-line-format nil)
  
  ;; Layout
  (setq fill-column 72)
  (setq-default line-spacing 1)

  ;; Indentation
  (setq org-startup-folded nil)  
  (org-indent-mode)
  (setq org-level-color-stars-only nil)
  (setq org-hide-leading-stars nil)
  (advice-add 'org-indent--compute-prefixes :override
              #'writer-mode--compute-prefixes)

  ;; Numbering
  (setq org-num-skip-unnumbered t)
  (setq org-num-skip-footnotes t)
  (setq org-num-max-level 2)
  (setq org-num-face nil)
  (org-num-mode)
  (setq org-num-format-function 'writer-mode--num-format))

(provide 'sumibi-writer)
