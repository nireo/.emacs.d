;; -*- lexical-binding: t; -*-

;; Enable the package manager
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Display the time it took when starting up emacs.
(defun display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                   (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'display-startup-time)

(defun reload-init-file ()
  (interactive)
  (load-file user-init-file)
  (princ "init file reloaded"))

(global-set-key (kbd "C-x C-l") 'reload-init-file)

;; Some performance boosters.
(setq read-process-output-max (* 3 1024 1024)) ;; 3mb
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 31457280 ; 32mb
          gc-cons-percentage 0.1)))

(defun nro/defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun nro/restore-garbage-collection-h ()
  ;; Defer it so that commands launched immediately after will enjoy the
  ;; benefits.
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

(defun nro/lsp-ui-doc-show ()
    (interactive)
    (lsp-ui-doc-hide)
    (lsp-ui-doc-show)
    (lsp-ui-doc-show))

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
  (evil-define-key 'normal 'global (kbd "<leader>he") 'shell)
  (evil-define-key 'normal 'global (kbd "<leader>fb") 'lsp-format-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>fr") 'lsp-format-region)
  (evil-define-key 'normal 'global (kbd "<leader>dsh") 'nro/lsp-ui-doc-show)
  (evil-define-key 'normal 'global (kbd "<leader>di") 'dired)
  (evil-define-key 'normal 'global (kbd "<leader>ne") 'flycheck-list-errors)
  (evil-define-key 'normal 'global (kbd "<leader>kb") 'kill-this-buffer)


  (evil-define-key 'normal 'global (kbd "<leader>na") 'org-cycle-agenda-files)

  (evil-define-key 'normal 'global (kbd "<leader>wr") 'split-window-right)
  (evil-define-key 'normal 'global (kbd "<leader>wb") 'split-window-below)
  (evil-define-key 'normal 'global (kbd "<leader>dw") 'delete-window)

  ;; Save a file.
  (evil-define-key 'normal 'global (kbd "<leader>s") 'save-buffer)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))



;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; Change indentation
(setq-default tab-width 2)
(setq-default standard-indent 2)
(setq-default electric-indent-inhibit t)
(setq-default indent-tabs-mode nil) ;; Don't use tabs since it seems to break the code when using github

;; Enable copypasting outside of emacs
(setq x-select-enable-clipboard t)

;; Disable ring-bell
(setq ring-bell-function 'ignore)

;; Show matching parenthesies
(show-paren-mode 1)

;; Prettify symbols
(global-prettify-symbols-mode t)				;

;; Use UTF-8
(set-language-environment "UTF-8")
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Set a warning when opening files larger than 100mb
(setq large-file-warning-threshold 100000000)


;; Disable cursor blinking
(blink-cursor-mode 0)

;; Better scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(setq echo-keystrokes 0.1)

(setq mode-line-position '(line-number-mode ("%l")))

;; Show empty scratch buffer and make the mode org mode
(setq initial-scratch-message nil)
(setq initial-major-mode 'org-mode)

;; Cleanup whitespaces
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Kill the current buffer rather than askin which buffer
(global-set-key (kbd "C-x k") 'kill-this-buffer)

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
  :config
  (elcord-mode)
  (setq elcord-use-major-mode-as-main-icon t))

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

;; Set up the visible bell
(setq visible-bell t)

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic nil) ; if nil, italics is universally disabled
    (load-theme 'doom-sourcerer t)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Add line number display
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

(setq display-line-numbers-type 'relative)

;; Add line wrapping
(global-visual-line-mode 1)

;; Remove the startup message
(setq inhibit-startup-message t)
(set-fringe-mode 10)

;; Set font
(set-face-attribute 'default nil :font "Monospace" :weight 'normal :height 140)
(set-face-attribute 'variable-pitch nil :family "Monospace" :weight 'normal :height 165)

;; Projectile configuration
(use-package projectile
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

(require 'lsp-mode)
(add-hook 'go-mode-hook #'lsp-deferred)

;; Since clangd in quite fast
(setq lsp-idle-delay 0.1)
(setq lsp-log-io nil)

(setq lsp-keymap-prefix "C-c l")


;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Add support for rust
(use-package rustic
  :config
  (setq lsp-eldoc-hook nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-signature-auto-activate nil))

(use-package flycheck
  :ensure t)
(setq lsp-rust-analyzer-server-display-inlay-hints t)

;; Add support for toml files which rust uses for configuration
(use-package toml-mode)

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

;; Make the user confirm that they're closing emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; Disable the warning when closing processes
(setq confirm-kill-processes nil)

;; Newline at the of file
(setq require-final-newline t)

;; Revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; Resize bindings
(global-set-key (kbd "s-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "s-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "s-C-<down>") 'shrink-window)
(global-set-key (kbd "s-C-<up>") 'enlarge-window)

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

;; Git integration
(use-package magit
  :bind (("C-x g" . magit-status)))

;; So I don't have to type many things twice
(use-package smartparens
  :ensure t
  :init
  (smartparens-global-mode))

;; Better looks by adding more icons
(use-package all-the-icons)

;; Add the same icons when using dired
(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook #'all-the-icons-dired-mode)
  (setq all-the-icons-dired-monochrome nil))

(add-hook 'text-mode-hook
          'variable-pitch-mode)

(customize-set-variable 'org-blank-before-new-entry
                        '((heading . nil)
                          (plain-list-item . nil)))
(setq org-cycle-separator-lines 1)
(setq org-adapt-indentation nil)
(setq org-indent-indentation-per-level 1)

;; Add spell checking when using text mode.
(customize-set-variable 'ispell-program-name "aspell")
(customize-set-variable 'ispell-extra-args '("--sug-mode=ultra"))
(add-hook 'text-mode-hook 'flyspell-mode)

;; Add writegood mode for better quality writing.
(require 'writegood-mode)
(global-set-key "\C-cg" 'writegood-mode)
(global-set-key "\C-c\C-gg" 'writegood-grade-level)
(global-set-key "\C-c\C-ge" 'writegood-reading-ease)

(setq org-modules '(org-habit))
(eval-after-load 'org
  '(org-load-modules-maybe t))
(setq org-default-notes-file "~/docs/org/notes.org")
(setq org-habit-graph-column 80)
(setq org-habit-show-habits-only-for-today nil)


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

(require 'org-superstar)
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
(global-set-key (kbd "C-w") 'kill-this-buffer)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-x |") 'split-window-right)
(global-set-key (kbd "C-x _") 'split-window-below)
(global-set-key (kbd "C-x \\") 'delete-window)


;; I have this bad habit of pressing this key combination, and if it doesn't exist it opens a
;; mail window
(global-set-key (kbd "C-x m") 'shell)

;; Make QSC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(define-key global-map "\C-ca" 'org-agenda)

;; Get the other file
(add-hook 'c-mode-common-hook
  (lambda()
    (local-set-key  (kbd "C-c o") 'ff-find-other-file)))

;; Load a theme without all of questions
(advice-add 'load-theme
            :around
            (lambda (fn theme &optional no-confirm no-enable)
              (funcall fn theme t)))

;; Change to scratch buffer
(global-set-key (kbd "<f12>")
  (lambda()(interactive)
    (switch-to-buffer (get-buffer-create "*scratch*"))))

;; Edit config file
(defun nro/edit-config ()
  (interactive)
  (find-file (concat user-emacs-directory "init.el")))
(global-set-key (kbd "<f11>") 'nro/edit-config)

(defun insert-date ()
  "Insert today's date at point"
  (interactive "*")
  (insert (format-time-string "%F")))
(global-set-key (kbd "C-c C-.") #'insert-date)

;; Dired configuration
(require 'dired-x)

(setq-default dired-listing-switches "-alh")
(setq dired-recursive-copies 'always)
(setq dired-recursive-delets 'always)
(setq dired-ls-F-marks-symlinks t)

(use-package dired-subtree
  :config
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)
             ("r" . dired-subtree-remove)))

(use-package dired-narrow
  :ensure t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(require 'spaceline-config)
(setq powerline-default-separator 'wave)
(spaceline-helm-mode)
(spaceline-spacemacs-theme)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("0f7fa4835d02a927d7d738a0d2d464c38be079913f9d4aba9c97f054e67b8db9" "0e2a7e1e632dd38a8e0227d2227cb8849f877dd878afb8219cb6bcdd02068a52" "1623aa627fecd5877246f48199b8e2856647c99c6acdab506173f9bb8b0a41ac" default))
 '(helm-minibuffer-history-key "M-p")
 '(org-agenda-files '("~/docs/org/todo.org" "~/docs/org/habits.org"))
 '(package-selected-packages
   '(writegood-mode flycheck rustic spaceline doom-themes dired-subtree all-the-icons-dired toml-mode rust-mode org-superstar modus-themes elcord smartparens magit which-key helm-projectile projectile company lsp-ui lsp-mode go-mode use-package evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
