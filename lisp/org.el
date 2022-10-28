;; org.el --- Personal configuration file -*- lexical-binding: t; -*-

;;; Commentary:
;;; Org mode configuration

;;; Code:

;; Setup some things for better exports when using org mode.
(with-eval-after-load 'ox-latex
(add-to-list 'org-latex-classes
             '("org-plain-latex"
               "\\documentclass{article}
           [NO-DEFAULT-PACKAGES]
           [PACKAGES]
           [EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

;; Org agenda config
(setq-default org-agenda-window-setup 'current-window
              org-agenda-ski-unvailable-files t
              org-agenda-inhibit-startup t
              org-agenda-span 10
              org-agenda-start-on-weekday nil
              org-agenda-start-day "-3d")

;; Improve looks
(set-face-attribute 'fixed-pitch nil :font "Menlo")
(set-face-attribute 'variable-pitch nil :font "DejaVu Sans")

(setq org-startup-indented t
      org-pretty-entities t
      org-hide-emphasis-markers t)


;;; org.el ends here
