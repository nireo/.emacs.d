;; Disable the starting message
(setq inhibit-startup-message t)

;; Change up the UI
(scroll-bar-mode -1) ;; Disable the visible scroll bar
(tool-bar-mode -1) ;; Disable the toolbar
(tooltip-mode -1) ;; Disable tooltips
(set-fringe-mode 10)
(menu-bar-mode -1) ;; Disable the menubar

(setq visible-bell t) ;; Remove the beep

;; Set the font more readable
(set-face-attribute 'default nil :font "Source Code Pro" :height 160)

;; Load the emacs theme
(load-theme 'wombat)

;; Set line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package counsel)

;; Download Evil
(unless (package-installed-p 'evil)
   (package-install 'evil))

;; Enable Evil
(require 'evil)
(evil-mode 1)

(use-package ivy
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

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15))

(use-package rust-mode)
(setq rust-format-on-save t)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keyma-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(setq lsp-prefer-capf t)
(setq lsp-completion-provider :capf)
(setq lsp-completion-enable t)

(use-package lsp-ui-mode)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("18cd5a0173772cdaee5522b79c444acbc85f9a06055ec54bb91491173bc90aaa" default))
 '(package-selected-packages '(counsel ivy use-package)))
