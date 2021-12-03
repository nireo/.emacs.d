;; -*- lexical-binding: t; -*-
;; Enable the package manager
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(set-frame-parameter (selected-frame) 'alpha '(100 . 50))
(add-to-list 'default-frame-alist '(alpha . (100 . 50)))

;; Display the time it took when starting up emacs.
(defun display-startup-time ()
  "Display startup time when opening Emacs."
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                   (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'display-startup-time)

;; Some performance boosters.
(setq read-process-output-max (* 3 1024 1024)) ;; 3mb
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 31457280 ; 32mb
          gc-cons-percentage 0.1)))

(defun nro/defer-garbage-collection-h ()
  "Set the garbage collection threshold to 'most-positive-fixnum'."
  (setq gc-cons-threshold most-positive-fixnum))

;; Defer it so that commands launched immediately after will enjoy the
;; benefits.
(defun nro/restore-garbage-collection-h ()
  "Restore the garbage collection threshold back to a normal number."
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold 31457280))))

(add-hook 'minibuffer-setup-hook #'nro/defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'nro/restore-garbage-collection-h)

;; Emacs checks if a special handler is needed to read a certain file. But that is not needed
;; during startup. So we can temporarily disable it.
(defvar nro--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq file-name-handler-alist nro--file-name-handler-alist)))

;; Vim keybindings in emacs.
(use-package evil
  :init
  (evil-mode 1)
  :config
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (evil-set-leader 'normal (kbd "SPC"))
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)

  ;; Such that there is no need to use the ESC-key.
  (define-key evil-insert-state-map (kbd "C-j") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-f") 'evil-delete-backward-char-and-join)

  ;; Some keybindings for better window navigation
  (evil-define-key 'normal 'global (kbd "<leader>wj") 'evil-window-bottom)
  (evil-define-key 'normal 'global (kbd "<leader>wh") 'evil-window-left)
  (evil-define-key 'normal 'global (kbd "<leader>wl") 'evil-window-right)
  (evil-define-key 'normal 'global (kbd "<leader>wk") 'evil-window-up)

  ;; Other leader keybindings
  (evil-define-key 'normal 'global (kbd "<leader>ff") 'find-file)
  (evil-define-key 'normal 'global (kbd "<leader>fb") 'lsp-format-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>di") 'dired)
  (evil-define-key 'normal 'global (kbd "<leader>kb") 'kill-this-buffer)

  ;; Save a file.
  (evil-define-key 'normal 'global (kbd "<leader>s") 'save-buffer)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))


(use-package cmake-mode
  :ensure t
  :mode (("/CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

(use-package rainbow-delimiters
  :ensure t
  :init
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                vterm-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Change indentation
(setq-default tab-width 4)
(setq-default standard-indent 4)
(setq-default electric-indent-inhibit t)
(setq-default indent-tabs-mode nil) ;; Don't use tabs since it seems to break the code when using github

;; Enable copypasting outside of emacs
(setq select-enable-clipboard t)

;; Disable ring-bell
(setq ring-bell-function 'ignore)

;; Show matching parenthesies
(show-paren-mode 1)

;; Prettify symbols
(global-prettify-symbols-mode t)

;; Use UTF-8
(set-language-environment "UTF-8")
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; 4 left 0 right
(fringe-mode '(4 . 0))

;; Set a warning when opening files larger than 200mb
(setq large-file-warning-threshold 200000000)

;; Disable cursor blinking
(blink-cursor-mode 0)

;; Better scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(setq echo-keystrokes 0.1)
(setq require-final-newline t)

;; Show empty scratch buffer and make the mode org mode
(setq initial-scratch-message nil)
(setq initial-major-mode 'org-mode)

;; Cleanup whitespaces
(add-hook 'before-save-hook 'whitespace-cleanup)

(use-package helm
  :ensure t
  :config
  (helm-mode 1)
  (setq helm-autoresize-mode t)
  (setq helm-buffer-max-length 40)
  (global-set-key (kbd "M-x") #'helm-M-x)
  (define-key helm-map (kbd "S-SPC") 'helm-toggle-visible-mark)
  (define-key helm-find-files-map (kbd "C-k") 'helm-find-files-up-one-level))

;; helm-projectile configuration
(use-package helm-projectile
  :bind (("C-S-P" . helm-projectile-switch-project)
     :map evil-normal-state-map
     ("C-p" . helm-projectile))
  :ensure t)

;; Enable rich presense on discord and some configuration for it.
(use-package elcord
  :ensure t
  :config
  (elcord-mode)
  (setq elcord-use-major-mode-as-main-icon t)
  (setq elcord-quiet t))

;; Disable the menubar
(menu-bar-mode -1)

;; Disable the scroll bar
(scroll-bar-mode -1)

;; Disable the toolbar
(tool-bar-mode -1)

;; Disable tooltips
(tooltip-mode -1)

;; Add special highlighting for the current line
(global-hl-line-mode 1)

;; y or n instead of yes-or-no
(fset 'yes-or-no-p 'y-or-n-p)

;; Navigation in camel case words.
(global-subword-mode)

;; (load-theme 'base16-black-metal-immortal t)
(load-theme 'modus-vivendi t)

;; Stop saving backups since they're quite useless
(setq make-backup-files nil)

;; Stop auto saving files, since they're not needed
(setq auto-save-default nil)

;; Make the cursor the size of the underlying character.
(setq x-stretch-cursor t)

;; Enable the usage of the system clipboard.
(setq select-enable-clipboard t)
(setq x-select-enable-clipboard t)

;; Make the max width of a line to be 80 characters.
(setq fill-column 80)

;; Fix the window not being fullscreen and leaving a gap
(setq frame-resize-pixelwise t)

;; Set the title to be something other than emacs@hostname
(setq frame-title-format "%b - emacs")

;; When opening a file, always follow symlinks
(setq vc-follow-symlinks t)

;; Speed up line movement
(setq auto-window-vscroll nil)

;; Revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; Add line number display
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

(setq display-line-numbers-type 'relative)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Add line wrapping
(global-visual-line-mode 1)

;; Remove the startup message
(setq inhibit-startup-message t)
(set-fringe-mode 10)

;; Set font
(set-face-attribute 'default nil :font "JetBrains Mono Nerd Font" :weight 'medium :height 135)
(set-face-attribute 'variable-pitch nil :family "JetBrains Mono Nerd Font" :weight 'medium :height 135)

;; Projectile configuration
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; Company configuration
(use-package company
  :ensure t
  :config
  (global-company-mode)
  (setq company-idle-delay 0)
  (setq company-selection-wrap-around t)
  (define-key company-active-map [tab] 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

(global-set-key (kbd "TAB") #'company-indent-or-complete-common)

(use-package lsp-mode
  :ensure t)

(setq lsp-keymap-prefix "C-c l")
(define-key lsp-mode-map (kbd "C-c l") lsp-command-map)

(add-hook 'go-mode-hook #'lsp-deferred)

;; Since clangd in quite fast
(setq lsp-idle-delay 0.1)
(setq lsp-log-io nil)


;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Add support for rust
(use-package rustic
  :ensure t
  :config
  (setq lsp-eldoc-hook nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-signature-auto-activate nil))

(use-package eldoc
  :diminish
  :hook (after-init . global-eldoc-mode))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(define-key yas-keymap (kbd "<return>") 'yas/exit-all-snippets)

(use-package flycheck
  :ensure t
  :config
  (progn
    (global-flycheck-mode)))

;; Add support for toml files which rust uses for configuration
(use-package toml-mode
  :ensure t)

;; Add some visual elements to lsp mode.
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config (setq lsp-ui-sideline-enable nil
      lsp-ui-peek-enable t
        lsp-ui-doc-enable nil
        lsp-ui-flycheck-enable nil
    lsp-ui-sideline-enable t
        lsp-ui-imenu-enable t
        lsp-ui-sideline-ignore-duplicate t))


;; Configuration for Go LSP support
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-book #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
(add-hook 'go-mode-hook #'lsp-deferred)

;; For C++ LSP support
(which-key-mode)
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)


;; For markdown editing
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'"   . web-mode)
         ("\\.jsx?\\'"  . web-mode)
         ("\\.tsx?\\'"  . web-mode)
         ("\\.json\\'"  . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2) ; HTML
  (setq web-mode-css-indent-offset 2)    ; CSS
  (setq web-mode-code-indent-offset 2)   ; JS/JSX/TS/TSX
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))))

(defun web-mode-init-prettier-hook ()
  (add-node-modules-path)
  (prettier-js-mode))

(add-hook 'web-mode-hook  'web-mode-init-prettier-hook)

(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint json-jsonlist)))

(use-package typescript-mode
  :ensure t
  :config
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode #'subword-mode))

(use-package json-mode
  :ensure t
  :mode (("\\.json\\'" . json-mode)))

(use-package vterm
  :ensure t)

(use-package vterm-toggle
  :ensure t)

;; Git integration
(use-package magit
  :ensure t
  :defer t
  :bind (("C-x g" . magit-status)))

;; So I don't have to type many things twice
(use-package smartparens
  :ensure t
  :init
  (smartparens-global-mode))

;; Better looks by adding more icons
(use-package all-the-icons
  :ensure t)

(add-hook 'text-mode-hook
          'variable-pitch-mode)

(customize-set-variable 'org-blank-before-new-entry
                        '((heading . nil)
                          (plain-list-item . nil)))
(setq org-cycle-separator-lines 1)
(setq org-adapt-indentation nil)

;; Add spell checking when using text mode.
;; (customize-set-variable 'ispell-program-name "aspell")
;; (customize-set-variable 'ispell-extra-args '("--sug-mode=ultra"))
;; (add-hook 'text-mode-hook 'flyspell-mode)

;; Setup some org-mode configuration.
(setq org-modules '(org-habit))
(eval-after-load 'org
  '(org-load-modules-maybe t))
(setq org-default-notes-file "~/docs/org/notes.org")
(setq org-habit-graph-column 80)

(defun org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil)
  (setq org-hide-emphasis-markers t))

(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                          (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(use-package org-superstar
  :ensure t)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

(use-package org
  :hook (org-mode . org-mode-setup))

(setq org-directory "~/docs/org")
(setq org-agenda-files '("~/docs/org/todo.org" "~/docs/org/habits.org"))

(add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'org-mode-hook 'visual-line-mode)

;; Add more TODO keywords.
(setq org-todo-keywords
      (quote ((sequence "TODO(t)"
                        "IN_PROGRESS(i!)"
                        "DONE(d!)"
                        "CANCELLED(c!)"
                        "POSTPONED(p!)"
                        ))))

;; Fast selection for todos
(setq org-use-fast-todo-selection t)

;; Use evil mode in org-mode
(use-package evil-org
  :ensure t
  :after (evil org)
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme '(navigation insert textobjects additional calendar))))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))


;; Custom key bindings
(global-set-key (kbd "C-/") 'comment-line)
(global-set-key (kbd "C-?") 'comment-or-uncomment-region)

;; I have this bad habit of pressing this key combination, and if it doesn't exist it opens a
;; mail window
(global-set-key (kbd "C-x m") 'vterm)

;; Make QSC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(define-key global-map "\C-ca" 'org-agenda)

;; Get the other file
(add-hook 'c-mode-common-hook
  (lambda()
    (local-set-key  (kbd "C-c o") 'ff-find-other-file)))

;; Dired configuration
(require 'dired-x)

(setq-default dired-listing-switches "-alh")
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(setq dired-ls-F-marks-symlinks t)

;; Increase/decrease text size in all buffers.
(use-package default-text-scale
  :ensure t
  :defer t)

(use-package no-littering
  :ensure t)

(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
    '(("FIXME" error bold)
      ("TODO" org-todo)
      ("DONE" org-done)
      ("NOTE" bold))))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package yaml-mode
  :mode ("\\.\\(yml\\|yaml\\|\\config\\|sls\\)$" . yaml-mode)
  :ensure yaml-mode
  :defer t)

(use-package emmet-mode
  :custom
  (emmet-move-cursor-between-quotes t)
  :custom-face
  (emmet-preview-input ((t (:inherit lazy-highlight))))
  :hook
  (web-mode . emmet-mode)
  (css-mode . emmet-mode))

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(use-package dockerfile-mode
  :ensure t)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(use-package modern-cpp-font-lock
  :ensure t)
(modern-c++-font-lock-global-mode t)


;; Custom functions ------------------------
(defun code-compile ()
  "Compiles c++ and c code"
  (interactive)
  (unless (file-exists-p "Makefile")
    (set (make-local-variable 'compile-command)
     (let ((file (file-name-nondirectory buffer-file-name)))
       (format "%s -o %s %s"
           (if  (equal (file-name-extension file) "cpp") "g++" "gcc" )
           (file-name-sans-extension file)
           file)))
    (compile compile-command)))
(global-set-key [f9] 'code-compile)

(defun nro/switch-theme (theme)
  "Disable active themes and load THEME."
  (interactive (list (intern (completing-read "Theme: "
                               (->> (custom-available-themes)
                                 (-map #'symbol-name))))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme 'no-confirm))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun kill-dired-buffers ()
  "Kill all open dired buffers."
  (interactive)
  (mapc (lambda (buffer)
          (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer)))
        (buffer-list)))

(defun open-in-browser ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

(defun move-file ()
  "Write this file to a new location, and delete the old one."
  (interactive)
  (let ((old-location (buffer-file-name)))
    (call-interactively #'write-file)
    (when old-location
      (delete-file old-location))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#181818" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-safe-themes '(default))
 '(helm-minibuffer-history-key "M-p")
 '(iackage-selected-packages
   '(doom-modeline plan9-theme anti-zenburn-theme smart-mode-line-powerline-theme yaml-mode smart-mode-line prettier-js doom-themes yasnippet-snippets cmake-mode rainbow-delimiters default-text-scale wc-mode writegood-mode flycheck rustic spaceline dired-subtree all-the-icons-dired toml-mode org-superstar modus-themes elcord smartparens magit which-key helm-projectile projectile company lsp-ui lsp-mode go-mode use-package evil))
 '(ispell-extra-args '("--sug-mode=ultra"))
 '(ispell-program-name "aspell")
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(org-agenda-files '("~/docs/org/todo.org" "~/docs/org/habits.org"))
 '(org-blank-before-new-entry '((heading) (plain-list-item)))
 '(package-selected-packages
   '(yasnippet-snippets yaml-mode which-key web-mode wc-mode vterm-toggle use-package typescript-mode toml-mode smartparens rustic rust-mode rainbow-mode rainbow-delimiters prettier-js pfuture persp-mode pdf-tools page-break-lines org-superstar org-roam no-littering modern-cpp-font-lock memoize magit lsp-ui json-reformat hydra hl-todo helm-projectile go-mode flycheck evil-org evil-collection emacsql-sqlite3 elcord doom-themes doom-modeline dockerfile-mode docker dired-subtree dired-narrow default-text-scale dashboard cmake-mode cfrs base16-theme all-the-icons-dired ace-window))
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   '((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3")))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(emmet-preview-input ((t (:inherit lazy-highlight)))))
