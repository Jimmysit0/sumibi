;; -*- lexical-binding: t; -*-
;; sumibi-faces.el
;;
;; This file defines the 8 basic faces:
;;
;; - sumibi-face-critical  - sumibi-face-popout   - sumibi-face-salient
;; - sumibi-face-default   - sumibi-face-faded    - sumibi-face-subtle
;; - sumibi-cursor         - sumibi-face-modeline-ok
;;
;;; Code:

;; A theme is fully defined by these eight faces
(defgroup sumibi nil
  "Faces for the sumibi emacs theme")

(defface sumibi-face-default nil
  "Default face is used for regular information."
:group 'sumibi)

(defface sumibi-face-critical nil
  "Critical face is for information that requires immediate action.
It should be of high constrast when compared to other faces. This
can be realized (for example) by setting an intense background
color, typically a shade of red. It must be used scarcely."
:group 'sumibi)

(defface sumibi-face-popout nil
"Popout face is used for information that needs attention.
To achieve such effect, the hue of the face has to be
sufficiently different from other faces such that it attracts
attention through the popout effect."
:group 'sumibi)

(defface sumibi-face-strong nil
"Strong face is used for information of a structural nature.
It has to be the same color as the default color and only the
weight differs by one level (e.g., light/regular or
regular/bold). IT is generally used for titles, keywords,
directory, etc."
:group 'sumibi)

(defface sumibi-face-salient nil
"Salient face is used for information that are important.
To suggest the information is of the same nature but important,
the face uses a different hue with approximately the same
intensity as the default face. This is typically used for links."
:group 'sumibi)

(defface sumibi-face-faded nil
"Faded face is for information that are less important.
It is made by using the same hue as the default but with a lesser
intensity than the default. It can be used for comments,
secondary information and also replace italic (which is generally
abused anyway)."
:group 'sumibi)

(defface sumibi-face-subtle nil
"Subtle face is used to suggest a physical area on the screen.
It is important to not disturb too strongly the reading of
information and this can be made by setting a very light
background color that is barely perceptible."
:group 'sumibi)


;; Modeline

(defface sumibi-face-modeline-ok nil
"OK face in the modeline"
:group 'sumibi)

(defface sumibi-face-cursor nil
"Identifiying the cursor over the screen is important too"
:group 'sumibi)

(set-foreground-color sumibi-color-foreground)
(set-background-color sumibi-color-background)

(set-face-attribute 'default nil
                                        :foreground (face-foreground 'default)
                                        :background (face-background 'default))
(set-face-attribute 'sumibi-face-default nil
                                        :foreground (face-foreground 'default)
                                        :background (face-background 'default))
(set-face-attribute 'sumibi-face-critical nil
                                        :foreground (face-background 'default)
                                        :background sumibi-color-critical)
(set-face-attribute 'sumibi-face-popout nil
                                        :foreground sumibi-color-popout)

;; Modeline and Cursor
(set-face-attribute 'sumibi-face-modeline-ok nil
                                        :foreground sumibi-color-ok)
(set-face-attribute 'sumibi-face-cursor nil
                                        :foreground sumibi-cursor)

(if (display-graphic-p)
        (set-face-attribute 'sumibi-face-strong nil
                                                :foreground (face-foreground 'sumibi-face-default)
                                                :family "Space Mono"
                                                :weight 'light)
  (set-face-attribute 'sumibi-face-strong nil
                                                :foreground (face-foreground 'sumibi-face-default)
                                                :weight 'light))
  (set-face-attribute 'sumibi-face-salient nil
                                                :foreground sumibi-color-salient
                                                :weight 'light)
  (set-face-attribute 'sumibi-face-faded nil
                                                :foreground sumibi-color-faded
                                                :weight 'light)
  (set-face-attribute 'sumibi-face-subtle nil
                                                :background sumibi-color-subtle)

;; mode-line / header-line

(set-face-attribute 'mode-line nil
                                        :height 0.75
                                        :foreground (face-foreground 'sumibi-face-faded)
                                        :background (face-background 'sumibi-face-default)
                                        :overline nil
                                        :underline nil
                                        :box nil)
(set-face-attribute 'mode-line-inactive nil
                                        :height 0.75
                                        :foreground (face-foreground 'sumibi-face-faded)
                                        :background (face-background 'sumibi-face-default)
                                        :overline nil
                                        :underline nil
                                        :inherit nil
                                        :box nil)
(set-face-attribute 'header-line nil
                    :weight 'light
                    :foreground (face-foreground 'sumibi-face-default)
                    :background (face-background 'sumibi-face-faded)

                    :overline nil
                    :underline nil
                    :box nil
                    :box `(:line-width 1
                                       :color ,(face-background 'sumibi-face-default)
                                       :style nil)
                    :inherit nil)

(set-face-attribute 'internal-border nil
                    :background (face-background 'sumibi-face-default))

(if (display-graphic-p)
        (set-face-attribute 'bold nil :weight 'regular)
  (set-face-attribute 'bold nil :weight 'bold))

(provide 'sumibi-faces)
