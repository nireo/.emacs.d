;; Enable the package manager
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

;; ----  Evil mode settings
(use-package evil
	:config
	(evil-mode 1)
	(setq evil-vsplit-window-right t)
	(setq evil-split-window-below t)
	(evil-set-leader 'normal (kbd "SPC"))
	(define-key evil-insert-state-map (kbd "C-j") 'evil-normal-state)
	(define-key evil-insert-state-map (kbd "C-f") 'evil-delete-backward-char-and-join)

	(evil-define-key 'normal 'global (kbd "<leader>wj") 'evil-window-bottom)
	(evil-define-key 'normal 'global (kbd "<leader>wh") 'evil-window-left)
	(evil-define-key 'normal 'global (kbd "<leader>wl") 'evil-window-right)
	(evil-define-key 'normal 'global (kbd "<leader>wk") 'evil-window-up)
	(evil-define-key 'normal 'global (kbd "<leader>s") 'save-buffer)

	;; Use visual line motions even outside of visual-line-mode buffers
	(evil-global-set-key 'motion "j" 'evil-next-visual-line)
	(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

	(evil-set-initial-state 'messages-buffer-mode 'normal)
	(evil-set-initial-state 'dashboard-mode 'normal))

;; Improve performance
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 3 1024 1024)) ;; 1mb

(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(defvar better-gc-cons-threshold 134217728 ; 128mb
	"The default value to use for `gc-cons-threshold'.
If you experience freezing, decrease this.  If you experience stuttering, increase this.")

(add-hook 'emacs-startup-hook
					(lambda ()
						(setq gc-cons-threshold better-gc-cons-threshold)
						(setq file-name-handler-alist file-name-handler-alist-original)
						(makunbound 'file-name-handler-alist-original)))

;; Change indentation
(setq-default tab-width 2)
(setq-default standard-indent 2)
(setq-default electric-indent-inhibit t)
(setq-default indent-tabs-mode t)

;; Enable copypasting outside of emacs
(setq x-select-enable-clipboard t)

;; Disable ring-bell
(setq ring-bell-function 'ignore)

;; Show matching parenthesies
(show-paren-mode 1)

;; Use UTF-8
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; Change up the scrolling a bit
(setq scroll-conservatively 101)

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

;; Stop the cursor from blinking
(blink-cursor-mode -1)

;; y or n instead of yes-or-no
(fset 'yes-or-no-p 'y-or-n-p)

;; Use a custom theme
;; (load-theme 'monochrome t)
;; (load-theme 'anti-zenburn t)
(use-package kaolin-themes
	:config
	(load-theme 'kaolin-dark t)
	(kaolin-treemacs-theme))

;; Add line number display
(when (version<= "26.0.50" emacs-version )
	(global-display-line-numbers-mode))

;; Add line wrapping
(global-visual-line-mode 1)

;; Remove the startup message
(setq inhibit-startup-message t)
(set-fringe-mode 10)

;; Set font
(set-face-attribute 'default nil :font "Meslo LG S" :height 150)

;; Projectile configuration
(require 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

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

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
	(add-hook 'before-save-hook #'lsp-format-buffer t t)
	(add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

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

;; Add a simpler and cleaner bar compared to the default one
(use-package doom-modeline
	:ensure t
	:init (doom-modeline-mode 1)
	:custom ((doom-modeline-height 15)))

;; global key-binding settings for comment (jetbrains style)
(global-set-key (kbd "C-/") 'comment-line)
(global-set-key (kbd "C-?") 'comment-or-uncomment-region)

;; Resize bindings
(global-set-key (kbd "s-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "s-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "s-C-<down>") 'shrink-window)
(global-set-key (kbd "s-C-<up>") 'enlarge-window)

;; Git integration
(use-package magit
	:ensure t)

;; So I don't have to type many things twice
(use-package smartparens
	:ensure t
	:init
	(smartparens-global-mode))

(use-package all-the-icons)

(use-package treemacs
	:ensure t
	:defer t
	:init
	(with-eval-after-load 'winum
		(define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
	:config
	(progn
		(setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
					treemacs-deferred-git-apply-delay      0.5
					treemacs-directory-name-transformer    #'identity
					treemacs-display-in-side-window        t
					treemacs-eldoc-display                 t
					treemacs-file-event-delay              5000
					treemacs-file-extension-regex          treemacs-last-period-regex-value
					treemacs-file-follow-delay             0.2
					treemacs-file-name-transformer         #'identity
					treemacs-follow-after-init             t
					treemacs-git-command-pipe              ""
					treemacs-goto-tag-strategy             'refetch-index
					treemacs-indentation                   2
					treemacs-indentation-string            " "
					treemacs-is-never-other-window         nil
					treemacs-max-git-entries               5000
					treemacs-missing-project-action        'ask
					treemacs-move-forward-on-expand        nil
					treemacs-no-png-images                 nil
					treemacs-no-delete-other-windows       t
					treemacs-project-follow-cleanup        nil
					treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
					treemacs-position                      'left
					treemacs-read-string-input             'from-child-frame
					treemacs-recenter-distance             0.1
					treemacs-recenter-after-file-follow    nil
					treemacs-recenter-after-tag-follow     nil
					treemacs-recenter-after-project-jump   'always
					treemacs-recenter-after-project-expand 'on-distance
					treemacs-show-cursor                   nil
					treemacs-show-hidden-files             t
					treemacs-silent-filewatch              nil
					treemacs-silent-refresh                nil
					treemacs-sorting                       'alphabetic-asc
					treemacs-space-between-root-nodes      t
					treemacs-tag-follow-cleanup            t
					treemacs-tag-follow-delay              1.5
					treemacs-user-mode-line-format         nil
					treemacs-user-header-line-format       nil
					treemacs-width                         35
					treemacs-workspace-switch-cleanup      nil)

		;; The default width and height of the icons is 22 pixels. If you are
		;; using a Hi-DPI display, uncomment this to double the icon size.
		;;(treemacs-resize-icons 44)

		(treemacs-follow-mode t)
		(treemacs-filewatch-mode t)
		(treemacs-fringe-indicator-mode 'always)
		(pcase (cons (not (null (executable-find "git")))
								 (not (null treemacs-python-executable)))
			(`(t . t)
			 (treemacs-git-mode 'deferred))
			(`(t . _)
			 (treemacs-git-mode 'simple))))
	:bind
	(:map global-map
				("M-0"       . treemacs-select-window)
				("C-x t 1"   . treemacs-delete-other-windows)
				("C-x t t"   . treemacs)
				("C-x t B"   . treemacs-bookmark)
				("C-x t C-t" . treemacs-find-file)
				("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
	:after (treemacs evil)
	:ensure t)

(use-package treemacs-projectile
	:after (treemacs projectile)
	:ensure t)

(use-package treemacs-icons-dired
	:after (treemacs dired)
	:ensure t
	:config (treemacs-icons-dired-mode))

(use-package treemacs-magit
	:after (treemacs magit)
	:ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
	:after (treemacs persp-mode) ;;or perspective vs. persp-mode
	:ensure t
	:config (treemacs-set-scope-type 'Perspectives))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
	 '("0e2a7e1e632dd38a8e0227d2227cb8849f877dd878afb8219cb6bcdd02068a52" "1623aa627fecd5877246f48199b8e2856647c99c6acdab506173f9bb8b0a41ac" default))
 '(package-selected-packages
	 '(kaolin-themes treemacs tao-theme zenburn-theme acme-theme smartparens magit which-key doom-themes doom-modeline helm-projectile projectile company lsp-ui lsp-mode go-mode use-package evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
