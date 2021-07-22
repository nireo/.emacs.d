(defconst better-plain-colors
  '((white . ((background . "#ffffff")
        (foreground . "#000000")
        (weak   . "#888888")
        (weaker   . "#dddddd")
        (weakest    . "#efefef")
        (highlight  . "#fda50f")
        (warning    . "#cc241d")
        (success    . "#8ec07c")
        (string     . "#d3969b")))

    (gray .  ((background . "#151515")
        (foreground . "#ffffff")
        (weak   . "#aaaaaa")
        (weaker   . "#666666")
        (weakest    . "#222222")
        (highlight  . "#fda50f")
        (warning    . "#ff0000")
        (success    . "#00ff00")
        (string     . "#d3969b")))

(defmacro better-plain--variant-with-colors (variant &rest body)
  "Execute BODY in a scope where the different colors for given VARIANT is bound."
  `(let* ((colors (or (cdr (assoc ,variant better-plain-colors))
          (error "No such theme variant")))
    (background (cdr (assoc 'background colors)))
    (foreground (cdr (assoc 'foreground colors)))
    (weak       (cdr (assoc 'weak colors)))
    (weaker     (cdr (assoc 'weaker colors)))
    (weakest    (cdr (assoc 'weakest colors)))
    (highlight  (cdr (assoc 'highlight colors)))
    (warning    (cdr (assoc 'warning colors)))
    (success    (cdr (assoc 'success colors)))
    (string     (cdr (assoc 'string colors))))
     ,@body))

(defmacro better-plain--faces-spec ()
  "Provide the faces specification."
  (quote
   (mapcar
    (lambda (entry) (list (car entry) `((t ,@(cdr entry)))))
    `(

      ;; default
      (default (:background ,background :foreground ,foreground))
      (fringe  (:background ,background))
      (region  (:background ,highlight  :foreground ,foreground))
      (show-paren-match (:background ,background :foreground ,success :bold t))
      (show-paren-mismatch (:background ,background :foreground ,warning :bold t))
      (minibuffer-prompt (:weight bold :foreground ,foreground))
      (isearch (:background ,weak :foreground ,foreground :bold t))
      (lazy-highlight (:background ,weaker :foreground ,foreground))
      (link (:underline t))

      ;; mode line
      (mode-line (:box (:line-width -1 :color ,weaker)
           :background ,weakest :foreground ,foreground))

      (mode-line-inactive (:box (:line-width -1 :color ,weaker)
        :background ,background :foreground ,weaker))

      ;; font lock
      (font-lock-keyword-face (:bold t))
      (font-lock-function-name-face (:bold t))
      (font-lock-variable-name-face (:foreground ,foreground))
      (font-lock-warning-face (:foreground ,foreground :underline (:color ,warning :style wave)))
      (font-lock-builtin-face (:bold t))
      (font-lock-variable-name-face (:foreground ,foreground :italic t))
      (font-lock-constant-face (:bold t :italic t))
      (font-lock-type-face (:italic t))
      (font-lock-preprocessor-face (:italic t))
      (font-lock-comment-face (:foreground ,weak :italic t))
      (font-lock-string-face (:foreground ,string))
      (font-lock-doc-face (:inherit font-lock-comment-face))
      (line-number (:foreground ,weaker))
      (linum (:inherit line-number))
      (vertical-border (:foreground ,weaker))

      ;; eshell
      (eshell-prompt (:foreground ,foreground :bold t))
      (eshell-ls-directory (:foreground ,foreground :bold t))
      (eshell-ls-archive (:inherit eshell-ls-unreadable))
      (eshell-ls-backup (:inherit eshell-ls-unreadable))
      (eshell-ls-clutter (:inherit eshell-ls-unreadable))
      (eshell-ls-executable (:inherit eshell-ls-unreadable))
      (eshell-ls-missing (:inherit eshell-ls-unreadable))
      (eshell-ls-product (:inherit eshell-ls-unreadable))
      (eshell-ls-readonly (:inherit eshell-ls-unreadable))
      (eshell-ls-special (:inherit eshell-ls-unreadable))
      (eshell-ls-symlink (:inherit eshell-ls-unreadable))

      ;; company mode
      (company-tooltip (:background ,weakest :foreground ,foreground))
      (company-tooltip-selection (:background ,weaker :foreground ,foreground))
      ;;(company-tooltip-search (:background "#ff0000" :foreground "#00ff00"))
      (company-tooltip-common (:bold t))
      (company-tooltip-common-selection (:bold t))
      (company-scrollbar-bg (:background ,weaker))
      (company-scrollbar-fg (:background ,weak))
      (company-tooltip-annotation-selection (:background ,weaker :foreground ,foreground :italic t))
      (company-tooltip-annotation (:background ,weakest :foreground ,weak :italic t))

      ;; git gutter
      (git-gutter:modified (:background ,highlight :foreground ,highlight))
      (git-gutter:added (:background ,success :foreground ,success))
      (git-gutter:deleted (:background ,warning :foreground ,warning))

      ;; diff hl
      (diff-hl-change (:background ,highlight :foreground ,highlight))
      (diff-hl-insert (:background ,success :foreground ,success))
      (diff-hl-delete (:background ,warning :foreground ,warning))

      ;; hl line
      (hl-line (:background ,weakest))
      (highlight-current-line-face (:inherit hl-line))

      ;; ido
      (ido-first-match (:bold t))
      (ido-only-match (:bold t))
      (ido-subdir (:italic t))
      (ido-virtual (:foreground ,weak))
      (ido-vertical-match-face (:bold t :italic nil))

      ;; org mode
      (org-table (:foreground ,weak))

      ))))


(defun better-plain--variant-name (variant)
  (intern (format "betterplain-%s" (symbol-name variant))))

(defmacro better-plain--define-theme (variant)
  (let ((name (better-plain--variant-name variant))
        (doc (format "better plain theme (%s version)" variant)))
    `(progn
       (deftheme ,name ,doc)
       (put ',name 'theme-immediate t)
       (better-plain--variant-with-colors
        ',variant
        (apply 'custom-theme-set-faces ',name
               (better-plain--faces-spec)))
       (provide-theme ',name))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
         (file-name-as-directory (file-name-directory load-file-name))))

(provide 'better-plain-themes)
