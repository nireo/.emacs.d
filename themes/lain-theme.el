;;; lain-theme.el --- A simple dark theme for emacs  -*- lexical-binding: t -*-

(deftheme lain "A minimalist dark theme.")
(defgroup lain-theme nil
  "Simplicity theme."
  :group 'faces
  :prefix "lain-"
  :link '(url-link :tag "GitHub" "http://github.com/smallwat3r/emacs-lain-theme")
  :tag "Simplicity theme")

;;;###autoload
(eval-and-compile
  (defcustom lain-override-colors-alist '()
    "Place to override default theme colors.

You can override a subset of the theme's default colors by
defining them in this alist."
    :group 'lain-theme
    :type '(alist
            :key-type (string :tag "Name")
            :value-type (string :tag " Hex"))))

(eval-and-compile
  (defvar lain-default-colors-alist
    '(("lain-foreground"    . "#eeeeee")
      ("lain-background"    . "#000000")
      ("lain-comment"       . "#b45454")
      ("lain-string"        . "#b4a7d6")
      ("lain-white"         . "#ffffff")
      ("lain-grey-1"        . "#dddddd")
      ("lain-grey"          . "#595959")
      ("lain-grey+1"        . "#403D3D")
      ("lain-grey+2"        . "#1A1A1A")
      ("lain-yellow-1"      . "#fffcb9")
      ("lain-yellow"        . "#f2ff2a")
      ("lain-green"         . "#b6d7a8")
      ("lain-magenta"       . "#d7b3d8")
      ("lain-orange"        . "#ff5f00")
      ("lain-red"           . "#ff25a9")
      ("lain-cyan"          . "#97FFEB")
      ("lain-cyan+1"        . "#4a5791")
      ("lain-purple"        . "#420dab")
      ("lain-blue"          . "#aaddd2")
      ("lain-navy"          . "#010029"))
    "List of Simplicity colors."))

(defmacro lain-with-color-variables (&rest body)
  "Binds all lain colors around BODY."
  (declare (indent 0))
  `(let (,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   (append lain-default-colors-alist
                           lain-override-colors-alist)))
     ,@body))

(lain-with-color-variables
  (custom-theme-set-faces
   'lain
;;;;; General
   `(default
      ((t (:background ,lain-background
           :foreground ,lain-foreground))))
   `(cursor
     ((t (:background ,lain-cyan
          :foreground ,lain-background))))
   `(hl-line
     ((t (:background ,lain-background))))
   `(fringe
     ((t (:background ,lain-background))))
   `(success
     ((t (:foreground ,lain-green))))
   `(warning
     ((t (:foreground ,lain-orange))))
   `(error
     ((t (:foreground ,lain-red))))
   `(link
     ((t (:background ,lain-background
          :foreground ,lain-cyan+1
          :underline t))))
   `(link-visited
     ((t (:background ,lain-background
          :foreground ,lain-purple
          :underline t))))
   `(highlight
     ((t (:background ,lain-grey+2
          :foreground ,lain-white
          :weight bold))))
;;;;; Markdown
   `(markdown-pre-face
     ((t (:foreground ,lain-yellow-1))))
   `(markdown-inline-code-face
     ((t (:foreground ,lain-yellow-1))))
;;;;; Org
   `(org-code
     ((t (:foreground ,lain-yellow-1))))
   `(org-block-begin-line
     ((t (:background ,lain-navy
          :foreground ,lain-cyan+1))))
   `(org-block-end-line
     ((t (:background ,lain-navy
          :foreground ,lain-cyan+1))))
;;;;; Modeline
   `(mode-line-inactive
     ((t (:box nil
          :foreground ,lain-background
          :background ,lain-grey))))
   `(mode-line
     ((t (:box nil
          :foreground ,lain-background
          :background ,lain-grey-1))))
;;;;; Search
   `(isearch
     ((t (:foreground ,lain-navy
          :background ,lain-yellow-1
          :weight bold))))
   `(isearch-fail
     ((t (:foreground ,lain-red
          :weight bold))))
   `(lazy-highlight
     ((t (:foreground ,lain-navy
          :background ,lain-yellow-1
          :weight bold))))
   `(pdf-isearch-match
     ((t (:foreground ,lain-navy
          :background ,lain-cyan
          :weight bold))))
;;;;; XML
   `(nxml-element-local-name
     ((t (:foreground ,lain-yellow-1))))
   `(nxml-tag-delimiter
     ((t (:foreground ,lain-yellow-1))))
;;;;; Web-mode
   `(web-mode-html-tag-face
     ((t (:foreground ,lain-yellow-1))))
   `(web-mode-html-tag-bracket-face
     ((t (:foreground ,lain-yellow-1))))
   `(web-mode-html-attr-name-face
     ((t (:foreground ,lain-foreground))))
;;;;; Dired search prompt
   `(minibuffer-prompt
     ((t (:foreground ,lain-foreground))))
;;;;; Line number
   `(line-number
     ((t (:background nil
          :foreground ,lain-grey))))
   `(line-number-current-line
     ((t (:background nil
          :foreground ,lain-white :weight bold))))
;;;;; Highlight region color
   `(region
     ((t (:foreground ,lain-foreground
          :background ,lain-grey+1))))

   `(font-lock-preprocessor-face ((t (:foreground ,lain-white ))))
;;;;; Builtins
   `(font-lock-builtin-face
     ((t (:foreground ,lain-foreground))))
;;;;; Constants
   `(font-lock-constant-face
     ((t (:foreground ,lain-foreground))))
;;;;; Comments
   `(font-lock-comment-face
     ((t (:foreground ,lain-comment :weight bold))))
   `(font-lock-doc-face
     ((t (:foreground ,lain-comment :weight bold))))
;;;;; Function names
   `(font-lock-function-name-face
     ((t (:foreground ,lain-foreground))))
;;;;; Keywords
   `(font-lock-keyword-face
     ((t (:foreground ,lain-white :weight bold))))
;;;;; Strings
   `(font-lock-string-face
     ((t (:foreground ,lain-string :weight bold))))
;;;;; Variables
   `(font-lock-variable-name-face
     ((t (:foreground ,lain-foreground))))
   `(font-lock-type-face
     ((t (:foreground ,lain-white :weight bold))))
   `(font-lock-warning-face
     ((t (:foreground ,lain-red :weight bold))))
;;;;; Parenthesis
   `(show-paren-match
     ((t (:foreground ,lain-navy
          :background ,lain-cyan
          :underline (:color ,lain-cyan)
          :weight bold))))
   `(show-paren-mismatch
     ((t (:background ,lain-red
          :foreground ,lain-foreground
          :underline (:color ,lain-red)
          :weight bold))))
;;;;; sh
   `(sh-quoted-exec
     ((t (:foreground ,lain-yellow-1 :weight bold))))
;;;;; Ivy
   `(ivy-current-match
     ((t (:background ,lain-background
          :foreground ,lain-white
          :underline (:color ,lain-cyan)
          :weight bold))))
   `(ivy-minibuffer-match-highlight
     ((t (:inherit ivy-current-match
          :background ,lain-background
          :foreground ,lain-foreground
          :weight bold))))
   `(ivy-yanked-word
     ((t (:inherit ivy-current-match
          :background ,lain-yellow-1))))
   `(ivy-minibuffer-match-face-1
     ((t (:inherit ivy-current-match
          :background ,lain-background))))
   `(ivy-minibuffer-match-face-2
     ((t (:inherit ivy-current-match
          :background ,lain-background
          :foreground ,lain-cyan))))
   `(ivy-minibuffer-match-face-3
     ((t (:inherit ivy-current-match
          :background ,lain-background
          :foreground ,lain-green))))
   `(ivy-minibuffer-match-face-4
     ((t (:inherit ivy-current-match
          :background ,lain-background
          :foreground ,lain-yellow))))
;;;;; Ivy-posframe
   `(ivy-posframe
     ((t (:background ,lain-navy
          :foreground ,lain-foreground))))
   `(ivy-posframe-border
     ((t (:background ,lain-cyan))))
;;;;; Swiper
   `(swiper-line-face
     ((t (:background ,lain-navy
          :foreground ,lain-foreground
          :weight bold))))
;;;;; Eshell
   `(eshell-syntax-highlighting-alias-face
     ((t (:background ,lain-background
          :foreground ,lain-cyan))))
   `(eshell-syntax-highlighting-command-face
     ((t (:background ,lain-background
          :foreground ,lain-green))))
;;;;; Flycheck
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,lain-red)
        :inherit unspecified))
      (t (:foreground ,lain-red
          :weight bold
          :underline t))))
   `(flycheck-error-list-error
     ((t (:foreground ,lain-red))))
   `(flycheck-fringe-error
     ((t (:foreground ,lain-red))))
   `(flycheck-posframe-error-face
     ((t (:foreground ,lain-red))))
   `(flycheck-error-list-error-message
     ((t (:foreground ,lain-red))))
   `(flycheck-info
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,lain-cyan)
        :inherit unspecified))
      (t (:foreground ,lain-cyan
          :weight bold
          :underline t))))
   `(flycheck-error-list-info
     ((t (:foreground ,lain-cyan))))
   `(flycheck-fringe-info
     ((t (:foreground ,lain-cyan))))
   `(flycheck-posframe-info-face
     ((t (:foreground ,lain-cyan))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,lain-yellow)
        :inherit unspecified))
      (t (:foreground ,lain-yellow
          :weight bold
          :underline t))))
   `(flycheck-error-list-warning
     ((t (:foreground ,lain-yellow))))
   `(flycheck-fringe-warning
     ((t (:foreground ,lain-yellow))))
   `(flycheck-posframe-warning-face
     ((t (:foreground ,lain-yellow))))
;;;;; Company
   `(company-tooltip
     ((t (:background ,lain-grey+2
          :foreground ,lain-foreground))))
   `(company-tooltip-common
     ((t (:inherit company-tooltip
          :foreground ,lain-white
          :weight bold))))
   `(company-tooltip-selection
     ((t (:foreground ,lain-white
          :background ,lain-grey+1
          :weight bold))))
   `(company-tooltip-common-selection
     ((t (:inherit company-tooltip-selection))))
   `(company-tooltip-annotation
     ((t (:inherit company-tooltip
          :foreground ,lain-string))))
   `(company-tooltip-annotation-selection
     ((t (:inherit company-tooltip-selection))))
   `(company-scrollbar-fg
     ((t (:background ,lain-grey))))
   `(company-scrollbar-bg
     ((t (:background ,lain-string))))
   `(company-preview
     ((t (:foreground ,lain-cyan
          :background ,lain-background
          :slant italic))))
   `(company-preview-common
     ((t (:foreground ,lain-cyan
          :background ,lain-background
          :slant italic))))
;;;;; vTerm
   `(vterm-color-black
     ((t (:background "gray35"
          :foreground "gray35"))))
   `(vterm-color-blue
     ((t (:background ,lain-blue
          :foreground ,lain-blue))))
   `(vterm-color-cyan
     ((t (:background ,lain-cyan
          :foreground ,lain-cyan))))
   `(vterm-color-default
     ((t (:background ,lain-background
          :foreground ,lain-foreground))))
   `(vterm-color-green
     ((t (:background ,lain-green
          :foreground ,lain-green))))
   `(vterm-color-inverse-video
     ((t (:background ,lain-background
          :inverse-video t))))
   `(vterm-color-magenta
     ((t (:background ,lain-magenta
          :foreground ,lain-magenta))))
   `(vterm-color-red
     ((t (:background ,lain-red
          :foreground ,lain-red))))
   `(vterm-color-white
     ((t (:background "gray65"
          :foreground "gray65"))))
   `(vterm-color-yellow
     ((t (:background ,lain-yellow
          :foreground ,lain-yellow))))
;;;;; term
   `(term-color-black
     ((t (:background "gray35"
          :foreground "gray35"))))
   `(term-color-blue
     ((t (:background ,lain-blue
          :foreground ,lain-blue))))
   `(term-color-cyan
     ((t (:background ,lain-cyan
          :foreground ,lain-cyan))))
   `(term-color-green
     ((t (:background ,lain-green
          :foreground ,lain-green))))
   `(term-color-magenta
     ((t (:background ,lain-magenta
          :foreground ,lain-magenta))))
   `(term-color-red
     ((t (:background ,lain-red
          :foreground ,lain-red))))
   `(term-color-white
     ((t (:background "gray65"
          :foreground "gray65"))))
   `(term-color-yellow
     ((t (:background ,lain-yellow
          :foreground ,lain-yellow))))
   `(term-default-fg-color
     ((t (:inherit term-color-white))))
   `(term-default-bg-color
     ((t (:inherit term-color-black))))
;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face
     ((t (:foreground ,lain-blue))))
   `(rainbow-delimiters-depth-2-face
     ((t (:foreground ,lain-green))))
   `(rainbow-delimiters-depth-3-face
     ((t (:foreground ,lain-magenta))))
   `(rainbow-delimiters-depth-4-face
     ((t (:foreground ,lain-cyan))))
   `(rainbow-delimiters-depth-5-face
     ((t (:foreground ,lain-yellow))))
   `(rainbow-delimiters-depth-6-face
     ((t (:foreground ,lain-orange))))
   `(rainbow-delimiters-depth-7-face
     ((t (:foreground ,lain-red))))
   `(rainbow-delimiters-depth-8-face
     ((t (:foreground ,lain-blue))))
   `(rainbow-delimiters-depth-9-face
     ((t (:foreground ,lain-green))))
   `(rainbow-delimiters-mismatched-face
     ((t (:inherit show-paren-mismatch))))
   `(rainbow-delimiters-unmatched-face
     ((t (:inherit show-paren-mismatch))))
;;;;; dired
   `(dired-directory
     ((t (:foreground ,lain-orange))))
   `(dired-flagged
     ((t (:background ,lain-background
          :foreground ,lain-magenta))))
   `(dired-header
     ((t (:background ,lain-background
          :foreground ,lain-yellow))))
;;;;; diredfl
   `(diredfl-dir-name
     ((t (:inherit dired-directory))))
   `(diredfl-dir-heading
     ((t (:inherit dired-header))))
   `(diredfl-exec-priv
     ((t (:background ,lain-background
          :foreground ,lain-red))))
   `(diredfl-executable-tag
     ((t (:background ,lain-background
          :foreground ,lain-cyan))))
   `(diredfl-read-priv
     ((t (:background ,lain-background
          :foreground ,lain-green))))
   `(diredfl-write-priv
     ((t (:background ,lain-background
          :foreground ,lain-magenta))))
   `(diredfl-no-priv
     ((t (:background ,lain-background
          :foreground ,lain-foreground))))
   `(diredfl-rare-priv
     ((t (:background ,lain-background
          :foreground ,lain-magenta))))
   `(diredfl-other-priv
     ((t (:background ,lain-background
          :foreground ,lain-yellow))))
   `(diredfl-dir-priv
     ((t (:background ,lain-background
          :foreground ,lain-orange))))
   `(diredfl-date-time
     ((t (:background ,lain-background
          :foreground ,lain-foreground))))
   `(diredfl-file-name
     ((t (:background ,lain-background
          :foreground ,lain-foreground))))
   `(diredfl-file-suffix
     ((t (:background ,lain-background
          :foreground ,lain-yellow))))
   `(diredfl-number
     ((t (:background ,lain-background
          :foreground ,lain-foreground))))
   `(diredfl-flag-mark
     ((t (:background ,lain-background
          :foreground ,lain-magenta))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name)))
  (when (not window-system)
    (custom-set-faces '(default ((t (:background nil)))))))

(provide-theme 'lain)

;;; lain-theme.el ends here
