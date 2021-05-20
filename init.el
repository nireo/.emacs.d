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

(defun display-startup-time ()
	(message "Emacs loaded in %s with %d garbage collections."
					 (format "%.2f seconds"
									 (float-time
									 (time-subtract after-init-time before-init-time)))
					 gcs-done))

(add-hook 'emacs-startup-hook #'display-startup-time)

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
	(evil-set-initial-state 'dashboard-mode 'normal)

	(setq evil-insert-state-cursor 'hbar))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
								term-mode-hook
								shell-mode-hook
								eshell-mode-hook))
	(add-hook mode (lambda () (display-line-numbers-mode 0))))

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

;; Disable cursor blinking
(blink-cursor-mode 0)


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

(require 'elcord)
(elcord-mode)

(setq elcord-use-major-mode-as-main-icon t)

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

;; Use a custom theme
;; (load-theme 'kaolin-temple t)
(use-package modus-themes
	:ensure
	:init
	;; Add all your customizations prior to loading the themes
	(setq modus-themes-slanted-constructs t
				modus-themes-bold-constructs nil
				modus-themes-region 'no-extend)

	;; Load the theme files before enabling a theme
	(modus-themes-load-themes)
	:config
	;; Load the theme of your choice:
	(modus-themes-load-vivendi) ;; OR (modus-themes-load-vivendi)
	:bind ("<f5>" . modus-themes-toggle))

;; Add line number display
(when (version<= "26.0.50" emacs-version )
	(global-display-line-numbers-mode))

;; Add line wrapping
(global-visual-line-mode 1)

;; Remove the startup message
(setq inhibit-startup-message t)
(set-fringe-mode 10)

;; Set font
(set-face-attribute 'default nil :font "Monospace" :height 140)

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

;; global key-binding settings for comment (jetbrains style)
(global-set-key (kbd "C-/") 'comment-line)
(global-set-key (kbd "C-?") 'comment-or-uncomment-region)

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



;; Git integration
(use-package magit
	:ensure t)

;; So I don't have to type many things twice
(use-package smartparens
	:ensure t
	:init
	(smartparens-global-mode))

;; Better looks by adding more icons
(use-package all-the-icons)

(defun org-mode-setup ()
	(org-indent-mode)
	(variable-pitch-mode 1)
	(auto-fill-mode 0)
	(visual-line-mode 1)
	(setq evil-auto-indent nil))

(define-key global-map "\C-ca" 'org-agenda)

(use-package org
	:hook (org-mode . org-mode-setup)
	:config
	(setq org-ellipsis " ▾"
				org-hide-emphasis-markers t))

(use-package org-bullets
	:after org
	:hook (org-mode . org-bullets-mode)
	:custom
	(org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Replace list hyphen with dot
(font-lock-add-keywords 'org-mode
												'(("^ *\\([-]\\) "
													(0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(dolist (face '((org-level-1 . 1.2)
								(org-level-2 . 1.1)
								(org-level-3 . 1.05)
								(org-level-4 . 1.0)
								(org-level-5 . 1.1)
								(org-level-6 . 1.1)
								(org-level-7 . 1.1)
								(org-level-8 . 1.1))))

;; Make sure org-indent face is available
(require 'org-indent)

;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

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

(global-set-key (kbd "C-x m") 'shell)

(define-key global-map(kbd "C-+") 'text-scale-increase)
(define-key global-map(kbd "C--") 'text-scale-decrease)

;; Make sure the font size increases in every buffer
(defadvice text-scale-increase (around all-buffers (arg) activate)
	(dolist (buffer (buffer-list))
		(with-current-buffer buffer
			ad-do-it)))

;; Make sure the font size decreased in every buffer
(defadvice text-scale-decrease (around all-buffers (arg) activate)
	(dolist (buffer (buffer-list))
		(with-current-buffer buffer
			ad-do-it)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
	 '("0e2a7e1e632dd38a8e0227d2227cb8849f877dd878afb8219cb6bcdd02068a52" "1623aa627fecd5877246f48199b8e2856647c99c6acdab506173f9bb8b0a41ac" default))
 '(package-selected-packages
	 '(moe-theme modus-themes elcord telephone-line eziam-theme org-bullets kaolin-themes treemacs tao-theme zenburn-theme acme-theme smartparens magit which-key doom-themes doom-modeline helm-projectile projectile company lsp-ui lsp-mode go-mode use-package evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
