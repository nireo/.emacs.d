;;; init.el --- Personal configuration file -*- lexical-binding: t; -*-

;;; Commentary:
;;; Personal configuration files hosted at https://github.com/nireo/.emacs.d

;;; Code:
;; Enable the package manager
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(package-initialize)

;; Use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Display the time it took when starting up emacs.
(defun display-startup-time ()
  "Display startup time when opening Emacs."
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.4f seconds"
                   (float-time
                   (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'display-startup-time)

;; Read process is set to a really low value.
;; Setting it to a higher value increases performance especially for LSP-mode.
(setq read-process-output-max (* 3 1024 1024)) ;; 3mb

;; Change the garbage collector on startup
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 31457280 ; 32mb
          gc-cons-percentage 0.1)))

;; This is copied from doom emacs and it increases performance.
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

;; Font settings
(defvar nro/default-font-size 135)
(defvar nro/default-font "DejaVu Sans Mono") ;; Use the default monospace font set in fontconfig

(set-face-attribute 'default nil
                    :family nro/default-font
                    :height nro/default-font-size)

(font-lock-add-keywords 'org-mode
 '(("^ *\\([-]\\) "
 (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
(font-lock-add-keywords 'org-mode
 '(("^ *\\([+]\\) "
 (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "◦"))))))

;; Vim keybindings in emacs.
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (evil-mode 1)
  :config

  ;; Set leader key to space
  (evil-set-leader 'normal (kbd "SPC"))

  ;; Define different splits
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)

  ;; Such that there is no need to use the ESC-key.
  (define-key evil-insert-state-map (kbd "C-j") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-f") 'evil-normal-state)

  ;; Better marking keywords using the custom functions below.
  (evil-define-key 'normal 'global (kbd "<leader>n") 'nro/mark-word)
  (evil-define-key 'normal 'global (kbd "<leader>m") 'nro/mark-construct-dwim)

  ;; Open treemacs
  (evil-define-key 'normal 'global (kbd "<leader>t") 'treemacs)

  ;; Some keybindings for better window navigation
  (evil-define-key 'normal 'global (kbd "<leader>wj") 'evil-window-bottom)
  (evil-define-key 'normal 'global (kbd "<leader>wh") 'evil-window-left)
  (evil-define-key 'normal 'global (kbd "<leader>wl") 'evil-window-right)
  (evil-define-key 'normal 'global (kbd "<leader>wk") 'evil-window-up)

  ;; Other leader keybindings
  (evil-define-key 'normal 'global (kbd "<leader>p") 'find-file)
  (evil-define-key 'normal 'global (kbd "<leader>f") 'lsp-format-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>d") 'dired)
  (evil-define-key 'normal 'global (kbd "<leader>k") 'kill-this-buffer)

  ;; Save a file.
  (evil-define-key 'normal 'global (kbd "<leader>s") 'save-buffer)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;; (setq evil-insert-state-cursor 'hbar)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; Jump around more easily in evil normal mode.
(use-package evil-snipe
  :defer t
  :ensure t
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))

;; Evil support for more modes.
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; Add support for cmake files
(use-package cmake-mode
  :defer t
  :ensure t
  :mode (("/CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

;; Nicer buffer name buffers with same name
(use-package uniquify
  :defer 5
  :config
  (setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/"))

;; Add colored delimiters based on the depth of the delimiters.
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
(setq-default tab-width 2)
(setq-default standard-indent 2)
(setq-default electric-indent-inhibit t)
(setq-default indent-tabs-mode nil) ;; Don't use tabs since it seems to break the code when using github

(setq show-paren-delay 0.0)
(show-paren-mode 1) ;; Show matching parenthesies

;; Use UTF-8
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

(modify-coding-system-alist 'process "*" 'utf-8)

;; Treat clipboard input as UTF-8 string first; compound text next...
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(set-fringe-mode 0) ;; Disable fringes
(blink-cursor-mode -1) ;; Disable cursor blinking

(setq idle-update-delay 1.0)
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)
(setq fast-but-imprecise-scrolling t)
(setq jit-lock-defer-time 0)

;; Custom settings
(setq scroll-margin 0
      scroll-conservatively 101
      scroll-preserve-screen-position t
      scroll-down-aggressively 0.01
      scroll-up-aggressively 0.01
      echo-keystrokes 0.02
      require-final-newline t
      select-enable-clipboard t
      ring-bell-function 'ignore
      large-file-warning-threshold 100000000
      delete-by-moving-to-trash t
      help-window-select t ;; automatically select help windows, so that they can be deleted.
      confirm-kill-processes nil
      )

;; Update a buffer if a file changes on disk.
(global-auto-revert-mode 1)

;; Update changes into dired as well.
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; Cleanup whitespaces
(add-hook 'before-save-hook 'whitespace-cleanup)
(setq-default sentence-end-double-space nil)

;; A completion system for M-x and C-p
(use-package ivy
  :ensure t
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :ensure t
  :init
  (ivy-rich-mode 1))

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package counsel
  :diminish counsel-mode
  :ensure t
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

(menu-bar-mode -1) ;; Disable menubar
(scroll-bar-mode -1) ;; Disable scroll bar
(tool-bar-mode -1) ;; Disable toolbar
(tooltip-mode -1) ;; Disable tooltips
;; (global-hl-line-mode 1) ;; Highlight the line where the cursor is
(fset 'yes-or-no-p 'y-or-n-p) ;; Shorten yes-or-no questions
(global-subword-mode) ;; Make it so that 'w' in evil moves to the next camel case word

(setq-default prettify-symbols-alist '(("lambda" . ?λ)
                                       ("delta" . ?Δ)
                                       ("gamma" . ?Γ)
                                       ("phi" . ?φ)
                                       ("psi" . ?ψ)))

(setq make-backup-files nil) ;; Stop saving backups since they're quite useless in the modern age
(setq create-lockfiles nil) ;; Don't create lock files.
(setq auto-save-default nil) ;; Stop auto saving files, since they're not needed
(setq delete-old-versions t)

(setq x-stretch-cursor t) ;; Make the cursor the size of the underlying character.

;; Enable the usage of the system clipboard.
(setq select-enable-clipboard t)
(setq x-select-enable-clipboard t)
(setq fill-column 80) ;; Make the max width of a line to be 80 characters.
(setq frame-resize-pixelwise t) ;; Fix the window not being fullscreen and leaving a gap
(setq frame-title-format "%b - emacs") ;; Change hostname

(setq vc-follow-symlinks t) ;; When opening a file, always follow symlinks
(setq auto-window-vscroll nil) ;; Speed up line movement
(setq blink-matching-paren nil)
(setq use-dialog-box nil)
(global-auto-revert-mode t) ;; Revert buffers automatically when underlying files are changed externally
(setq undo-limit 100000000) ;; Increase undo limit

;; Add line number display
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;; Make numbers relative such that evil navigation is easier
(setq display-line-numbers-type 'relative)
(setq-default display-line-numbers-width 4)
(setq-default display-line-numbers-widen t)
(add-hook 'before-save-hook 'delete-trailing-whitespace) ;; Delete trailing whitespaces after saving
(global-visual-line-mode 1) ;; Add line wrapping
(setq inhibit-startup-message t) ;; Remove startup message

;; Projectile configuration
(use-package projectile
  :diminish projectile-mode
  :ensure t
  :custom ((projectile-completion-system 'ivy))
  :config
  (projectile-mode +1)

  ;; ignore some useless directories in cmake projects.
  (add-to-list 'projectile-globally-ignored-directories "build")
  (add-to-list 'projectile-globally-ignored-directories ".cache")
  (add-to-list 'projectile-globally-ignored-directories "cache")
  (add-to-list 'projectile-globally-ignored-directories "*clangd")

  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode)
  (setq counsel-find-file-ignore-regexp "\.|build")
  :bind ( :map evil-normal-state-map
     ("C-p" . counsel-projectile)))

;; Company configuration
(use-package company
  :diminish company-mode
  :ensure t
  :config
  (global-company-mode)
  (setq company-idle-delay 0)
  (setq company-selection-wrap-around t)
  (define-key company-active-map [tab] 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

(global-set-key (kbd "TAB") #'company-indent-or-complete-common)

(use-package company-box
  :diminish
  :hook (company-mode . company-box-mode))

;; Disable some lsp visuals
(defun nro/lsp-mode-setup ()
  (setq lsp-lens-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil))

(use-package lsp-mode
  :hook (lsp-mode . nro/lsp-mode-setup)
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l"))

(add-hook 'go-mode-hook #'lsp-deferred)

(setq lsp-idle-delay 0.1) ;; Change delay since most of the LSP are fast.
(setq lsp-log-io nil)

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
;; Add support for rust
(use-package rustic
  :defer t
  :ensure t
  :config
  (setq lsp-eldoc-hook nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-signature-auto-activate nil))

(use-package eldoc
  :diminish
  :hook (after-init . global-eldoc-mode))

;; Different snippets to help with coding faster
(use-package yasnippet
  :defer 15
  :diminish yas-minor-mode
  :ensure t
  :config
  (yas-global-mode 1)
  (define-key yas-keymap (kbd "<return>") 'yas/exit-all-snippets))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(use-package flycheck
  :ensure t
  :config
  (progn
    (global-flycheck-mode)))

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
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

;; Add LSP to C and C++ modes
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

;; For markdown editing
(use-package markdown-mode
  :defer t
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

;; Support for typescript
(use-package typescript-mode
  :defer t
  :mode "\\.tsx?\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

;; Support for python
(use-package python-mode
  :defer t
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  (python-shell-interpreter "python3"))

(defun web-mode-init-prettier-hook ()
  (add-node-modules-path)
  (prettier-js-mode))

(add-hook 'web-mode-hook  'web-mode-init-prettier-hook)
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint json-jsonlist)))

;; Support for json
(use-package json-mode
  :defer t
  :ensure t
  :mode (("\\.json\\'" . json-mode))
  :hook (before-save . nro/json-mode-before-save-hook)
  :preface
  (defun nro/json-mode-before-save-hook ()
    (when (eq major-mode 'json-mode)
      (json-pretty-print-buffer))))

;; A better terminal emulator as it isn't written in elisp :P
(use-package vterm
  :defer t
  :ensure t
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000)
  (defun clean-vterm-window ()
    (hl-line-mode -1))
  :hook (vterm-mode . clean-vterm-window))

(use-package vterm-toggle
  :defer t
  :ensure t
  :custom
  (vterm-toggle-scope 'project)
  (vterm-toggle-fullscreen-p nil "Open vterm in another window.")
  :bind(:map vterm-mode-map
             ("s-t" . #'vterm)))

;; Git integration
(use-package magit
  :ensure t
  :defer t
  :bind (("C-x g" . magit-status)))

;; Completes parenthesies and other punctuators.
(use-package smartparens
  :defer t
  :ensure t
  :init
  (smartparens-global-mode))

;; Better looks by adding more icons
(use-package all-the-icons
  :defer t
  :ensure t)

(setq org-adapt-indentation nil)

;; Run different commands related to setting up org-mode
(defun nro/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1))

(use-package org-superstar
  :ensure t)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

(use-package org
  :hook (org-mode . nro/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (variable-pitch-mode 1)

  (setq org-hide-emphasis-markers t)
  (setq org-agenda-start-with-log-mode t)
  (setq org-use-fast-todo-selection t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-agenda-files "~/org/todo.org")
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

   (setq org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
      (sequence "PLAN(p)" "READY(r)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))
  (define-key global-map (kbd "C-c j")
    (lambda () (interactive) (org-capture nil "jj"))))


(use-package elixir-mode
  :defer t
  :config
  (use-package alchemist
    :defer t
    :hook ((elixir-mode . alchemist-mode)
           (elixir-mode . alchemist-phoenix-mode))))

;; Add easy commenting for lots of different languages
(use-package evil-nerd-commenter
  :defer t
  :ensure t
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

;; I have this bad habit of pressing this key combination, and if it doesn't exist it opens a
;; mail window
(global-set-key (kbd "C-x m") 'vterm)

;; Make QSC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(define-key global-map "\C-ca" 'org-agenda)

;; Resize windows with better bindings
(global-set-key (kbd "s-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "s-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "s-C-<down>") 'shrink-window)
(global-set-key (kbd "s-C-<up>") 'enlarge-window)

;; Get the other file
(add-hook 'c-mode-common-hook
  (lambda()
    (local-set-key  (kbd "C-c o") 'ff-find-other-file)))

;; Dired configuration
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom
  (dired-listing-switches "-agho --group-directories-first")
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  (dired-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  (dired-dwim-target t)
  (load-prefer-newer t)
  :config
  (put 'dired-find-alternate-file 'disabled nil))

(use-package dired-single)

(use-package dired-open
  :config
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("png" . "feh")
                                ("mkv" . "mpv"))))

;; Cleans up the emacs directory.
(use-package no-littering
  :ensure t)

(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; Highlight some important keywords
(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
    '(("FIXME" error bold)
      ("TODO" org-todo)
      ("DONE" org-done)
      ("NOTE" bold))))

;; Add support for the yaml configuration file
(use-package yaml-mode
  :delight "ψ"
  :mode ("\\.\\(yml\\|yaml\\|\\config\\|sls\\)$" . yaml-mode)
  :ensure yaml-mode
  :defer t)

;; Help with some css and html stuff to make programming in those languages easier.
(use-package emmet-mode
  :custom
  (emmet-move-cursor-between-quotes t)
  :custom-face
  (emmet-preview-input ((t (:inherit lazy-highlight))))
  :hook
  (web-mode . emmet-mode)
  (css-mode . emmet-mode))

;; A package to manage docker containers from emacs
(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

;; Add support to Dockerfiles, such that they have syntax highlighting
(use-package dockerfile-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

;; Syntax highlighting support for modern C++ syntax.
(use-package modern-cpp-font-lock
  :diminish
  :ensure t
  :init (modern-c++-font-lock-global-mode t))

;; A package which hides unnecessary minor-modes from the modeline.
(use-package diminish
  :ensure t)

;; Compile C++ and C code
(defun code-compile ()
  "Compiles C++ and C code."
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

;; Easily switch themes
(defun nro/switch-theme (theme)
  "Disable active themes and load THEME."
  (interactive (list (intern (completing-read "Theme: "
                               (->> (custom-available-themes)
                                 (-map #'symbol-name))))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme 'no-confirm))

;; Kill every other buffer other than the sellected one.
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun kill-dired-buffers ()
  "Kill all open Dired buffers."
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

(defadvice find-file (before make-directory-maybe (filename &optional wildcards)
                             activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir :make-parents)))))

;; Open current buffer file in the default browser.
(defun nro/open-in-file-browser (&optional file)
  "Open FILE or Dired marked file in external app."
  (interactive)
  (let ((file-list (if file
                       (list file)
                     (if (equal major-mode "dired-mode")
                         (dired-get-marked-files)
                       (list (buffer-file-name)))))
        (do-it-p   (if (<= (length file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when do-it-p
      (mapc (lambda (file-path)
              (let ((process-connection-type nil))
                (start-process "" nil "xdg-open" file-path)))
            file-list))))

;; Stage all changes made in git repository and commit those changes.
(defun nro/magit-stage-all-and-commit (message)
  "Stage every change, commit with MESSAGE, and push it."
  (interactive (list (progn (magit-diff-unstaged) (read-string "Commit Message: "))))
  (magit-stage-modified)
  (magit-commit-create (list "-m" message))
  (call-interactively #'magit-push-current-to-pushremote))

(defun nro/reload-config ()
  "Reloads configuration."
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defun nro/find-file-as-sudo ()
  "Find file as sudo."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (when file-name
      (find-alternate-file (concat "/sudo::" file-name)))))

(defun rename-this-file (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun kill-all-buffers ()
  "Kill all buffers in buffer list."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

;; Treemacs packages
(use-package treemacs
  :ensure t
  :defer t)

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

;; Support for ASM.
(use-package nasm-mode
  :defer 5
  :ensure t)

;; A package for focused editing, which adds a margin on the left and right side
;; of the text. Nice for writing.
(use-package olivetti
  :defer t
  :ensure t)

;; Load custom variables from a custom.el file, such that they don't clutter up
;; main init.el file.
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; It is more useful to just open up a todo file, compared to just being an empty elisp buffer.
(let ((filename "~/org/todo.org"))
  (when (file-exists-p filename)
    (setq initial-buffer-choice filename)))

(use-package scratch
  :ensure
  :config
  (defun nro/scratch-buffer-setup ()
    "Add contents to `scratch' buffer and name it accordingly."
    (let* ((mode (format "%s" major-mode))
           (string (concat "scratch buffer for: " mode "\n\n")))
      (when scratch-buffer
        (save-excursion
          (insert string)
          (goto-char (point-min))
          (comment-region (point-at-bol) (point-at-eol)))
        (forward-line 2))
      (rename-buffer (concat "*scratch for " mode "*") t)))
  :hook (scratch-create-buffer-hook . nro/scratch-buffer-setup)
  :bind ("C-c s" . scratch))

;; These marking functions are taken from:
;; https://protesilaos.com/codelog/2020-08-03-emacs-custom-functions-galore/
(use-package emacs
  :commands (nro/mark-symbol
             nro/mark-sexp-backward)
  :config
  (defmacro nro/mark (name object &optional docstring)
    `(defun ,name (&optional arg allow-extend)
       ,docstring
       (interactive "P\np")
       (let ((x (format "%s-%s" "forward" ,object)))
         (cond ((and allow-extend
                     (or (and (eq last-command this-command) (mark t))
                         (region-active-p)))
                (setq arg (if arg (prefix-numeric-value arg)
                            (if (< (mark) (point)) -1 1)))
                (set-mark
                 (save-excursion
                   (goto-char (mark))
                   (funcall (intern x) arg)
                   (point))))
               (t
                (let ((bounds (bounds-of-thing-at-point (intern ,object))))
                  (unless (consp bounds)
                    (error "No %s at point" ,object))
                  (if (>= (prefix-numeric-value arg) 0)
                      (goto-char (car bounds))
                    (goto-char (cdr bounds)))
                  (push-mark
                   (save-excursion
                     (funcall (intern x) (prefix-numeric-value arg))
                     (point)))
                  (activate-mark)))))))

  (nro/mark
   nro/mark-word
   "word"
   "Mark the whole word at point.
This function is a slightly modified version of the built-in
`mark-word', that I intend to use only in special circumstances,
such as when recording a keyboard macro where precision is
required.  For a general purpose utility, use `nro/mark-symbol'
instead.")

  (nro/mark
   nro/mark-symbol
   "symbol"
   "Mark the whole symbol at point.
With optional ARG, mark the current symbol and any remaining
ARGth symbols away from point.  A negative argument moves
backward. Repeated invocations of this command mark the next
symbol in the direction originally specified.

In the absence of a symbol and if a word is present at point,
this command will operate on it as described above.")

  (defun nro/mark-sexp-backward (&optional arg)
    (interactive "P")
    (if arg
        (mark-sexp (- arg) t)
      (mark-sexp (- 1) t)))

  (defun nro/mark-construct-dwim (&optional arg)
    (interactive "P")
    (cond
     ((symbol-at-point)
      (nro/mark-symbol arg t))
     ((eq (point) (cdr (bounds-of-thing-at-point 'sexp)))
      (nro/mark-sexp-backward arg))
     (t
      (mark-sexp arg t)))))

(use-package vundo
  :ensure t
  :defer t)

(use-package multiple-cursors
  :ensure t
  :bind (("M-." . mc/mark-next-like-this)
         ("M-," . mc/unmark-next-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

(use-package crux
  :ensure t
  :bind (("C-c o" . crux-open-with)))

(use-package ini-mode
  :defer t
  :ensure t
  :mode "\\.ini\\'")

(use-package delight
  :ensure t)

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :init
  (use-package ibuffer-vc
    :ensure t
    :commands (ibuffer-vc-set-filter-groups-by-vc-root)
    :custom
    (ibuffer-vc-skip-if-remote 'nil))
  :custom
  (ibuffer-formats
   '((mark modified read-only locked " "
           (name 35 35 :left :elide)
           " "
           (size 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " " filename-and-process)
     (mark " "
           (name 16 -1)
           " " filename))))

(defun nro/edit-config ()
  "Opens the Emacs configuration file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; Load custom version of the manoj-dark theme.
(load-theme 'manoj-custom t)

;;; init.el ends here
