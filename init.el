;; init.el --- Personal configuration file -*- lexical-binding: t; -*-

;;; Commentary:
;;; Personal configuration files hosted at https://github.com/nireo/.emacs.d

;;; Code:
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(package-initialize)

;;; use-package
;; a macro to simplify package management in emacs.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name)
(advice-add #'display-startup-echo-area-message :override #'ignore)

(setq initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

(define-advice load-file (:override (file) silence)
  (load file nil 'nomessage))

(defvar profile-dotemacs-file "~/.emacs.d" "File to be profiled.")
(define-advice startup--load-user-init-file (:before (&rest _) undo-silence)
      (advice-remove #'load-file #'load-file@silence))

(setq auto-mode-case-fold nil)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

(setq-default cursor-in-non-selected-windows nil)
(setq idle-update-delay 1.0)
(setq redisplay-skip-fontification-on-input t)

;; Avoid calling menu-bar-mode...etc because to do extra stuff that is not needed.
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; And set these to nil so users don't have to toggle the modes twice to
;; reactivate them.
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)
(setq default-input-method nil)
(setq-default fill-column 80)

(setq file-name-handler-alist-at-startup file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-at-startup)))

;; Show startup time.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2fs" (float-time (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Read process is set to a really low value.
;; Setting it to a higher value increases performance especially for LSP-mode.
(setq read-process-output-max (* 3 1024 1024)) ;; 3mb

;; Change the garbage collector on startup
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 31457280 ; 32mb
          gc-cons-percentage 0.1)))

;;; esup
;; tool to benchmark emacs start-up time.
(use-package esup
  :ensure t
  :config
  (setq esup-depth 0))

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

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; Font settings
(defvar nro/default-font-size 135)
(defvar nro/default-font "InconsolataGo Nerd Font")

(set-face-attribute 'default nil
                    :family nro/default-font
                    :height nro/default-font-size
                    :weight 'bold
                    )

;; ---- Emacs settings
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
(setq-default indent-tabs-mode nil) ;; Don't use tabs

(setq show-paren-delay 0.0) ;; Remove delay to display matching parenthesy
(show-paren-mode 1) ;; Show matching parenthesies

(modify-coding-system-alist 'process "*" 'utf-8)
(blink-cursor-mode -1) ;; Disable cursor blinking
(set-fringe-mode 0) ;; Hide fringes

;; Custom settings
(setq scroll-margin 0 ;; better scrolling
      scroll-conservatively 101 ;;-
      scroll-preserve-screen-position t ;;-
      scroll-down-aggressively 0.01 ;;-
      scroll-up-aggressively 0.01 ;;-
      fast-but-imprecise-scrolling t
      jit-lock-defer-time 0 ;; fontification is deferred when input is loading
      highlight-nonselected-windows nil
      echo-keystrokes 0.02
      require-final-newline t ;; newline at the end of files
      select-enable-clipboard t ;; make cutting and pasting use the clipboard
      ring-bell-function 'ignore ;; ignore
      large-file-warning-threshold 100000000 ;; increase the file warning threshold
      help-window-select t ;; automatically select help windows, so that they can be deleted.
      confirm-kill-processes nil ;; don't confirm when killing processes
      inhibit-compacting-font-caches t ;; don't trigger GC when loading larger fonts
      make-backup-files nil ;; Stop saving backups since they're quite useless in the modern age
      create-lockfiles nil ;; Don't create lock files.
      auto-save-default nil ;; Stop auto saving files, since they're not needed
      delete-old-versions t ;; Delete excess backups silently
      x-stretch-cursor t ;; Make the cursor the size of the underlying character.
      frame-resize-pixelwise t ;; Fix the window not being fullscreen and leaving a gap
      frame-title-format "%b - emacs" ;; change window title
      vc-follow-symlinks t ;; When opening a file, always follow symlinks
      vc-handled-backends nil
      auto-window-vscroll nil ;; Speed up line movement
      blink-matching-paren nil
      use-dialog-box nil
      undo-limit 100000000 ;; Increase undo limit

      ;; Make numbers relative such that evil navigation is easier
      display-line-numbers-type 'relative
      display-line-numbers-width 3
      display-line-numbers-widen t)

(global-auto-revert-mode 1) ;; Update a buffer if a file changes on disk.

;; Update changes into dired as well.
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; Cleanup whitespaces
(add-hook 'before-save-hook 'whitespace-cleanup)
(setq-default sentence-end-double-space nil)

(fset 'yes-or-no-p 'y-or-n-p) ;; Shorten yes-or-no questions
(global-subword-mode) ;; Make it so that 'w' in evil moves to the next camel case word
(global-auto-revert-mode t) ;; Revert buffers automatically when underlying files are changed externally

;; Add line number display
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

(add-hook 'before-save-hook 'delete-trailing-whitespace) ;; Delete trailing whitespaces after saving
(global-visual-line-mode 1) ;; Add line wrapping

;;; evil
;; key bindings from vim in emacs.
(use-package evil
  :ensure t
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

  (evil-define-key 'normal 'global (kbd "<leader>i") 'ibuffer)

  ;; Open treemacs
  (evil-define-key 'normal 'global (kbd "<leader>t") 'treemacs)

  ;; Some keybindings for better window navigation
  (evil-define-key 'normal 'global (kbd "<leader>wj") 'evil-window-bottom)
  (evil-define-key 'normal 'global (kbd "<leader>wh") 'evil-window-left)
  (evil-define-key 'normal 'global (kbd "<leader>wl") 'evil-window-right)
  (evil-define-key 'normal 'global (kbd "<leader>wk") 'evil-window-up)
  (evil-define-key 'normal 'global (kbd "<leader>fb") 'eglot-format-buffer)

  (evil-define-key 'normal 'global (kbd "<leader>l") 'consult-buffer)

  ;; Other leader keybindings
  (evil-define-key 'normal 'global (kbd "<leader>p") 'find-file)
  (evil-define-key 'normal 'global (kbd "<leader>d") 'dired)
  (evil-define-key 'normal 'global (kbd "<leader>k") 'kill-this-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>r") 'eglot-rename)
  (evil-define-key 'normal 'global (kbd "<leader>c") 'consult-projectile-find-file)

  ;; Save a file.
  (evil-define-key 'normal 'global (kbd "<leader>s") 'save-buffer)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (setq evil-insert-state-cursor 'box)

  (evil-set-initial-state 'messages-buffer-mode 'normal))

;;; evil-collection
;; more evil support for many different modes.
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;;; cmake-collection
;; syntax highlighting and support for cmake files.
(use-package cmake-mode
  :defer t
  :ensure t
  :mode (("/CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

;;; rainbow-delimeters
;; adding colored indicators for depth in parenthesies
(use-package rainbow-delimiters
  :ensure t
  :init
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;;; vertico
;; a minimal completion framework
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

;;; orderless
;; orderless completion
(use-package orderless
  :ensure t
  :custom (completion-styles '(orderless basic)))

;;; consult
;; consulting completing-read
(use-package consult
  :after projectile
  :ensure t
  :bind(
        ("C-x b" . consult-buffer)
        )
  :config
  (projectile-load-known-projects)
  (setq my-consult-source-projectile-projects
        `(:name "Projectile projects"
                :narrow   ?P
                :category project
                :action   ,#'projectile-switch-project-by-name
                :items    ,projectile-known-projects))
  (add-to-list 'consult-buffer-sources my-consult-source-projectile-projects 'append))

(use-package consult-projectile
  :ensure t
  :after consult)

;;; marginalia
;; add marginalia to minibuffer completions
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

;;; projectile
;; project management
(use-package projectile
  :diminish projectile-mode
  :config
  (progn
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    (projectile-mode +1)
    (setq projectile-completion-system 'default)
    (setq projectile-enable-caching t)
    (setq projectile-indexing-method 'alien)
    (add-to-list 'projectile-globally-ignored-files "node_modules")
    (add-to-list 'projectile-globally-ignored-files ".cache")
    (add-to-list 'projectile-globally-ignored-files "_cache")
    ))

;;; company
;; a text completion framework used with eglot to provide completion
;; when programming.
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

;;; company-box
;; an enhanced company interface.
(use-package company-box
  :ensure t
  :diminish
  :hook (company-mode . company-box-mode))

;;; eglot
;; simple language server interface for emacs. (simpler than lsp-mode)
(use-package eglot
  :ensure t
  :hook ((c-mode c++-mode go-mode rustic-mode) . eglot-ensure)
  :config
  (setq-default eglot-workspace-configuration
                '((:gopls .
                          ((staticcheck . t))))))

(require 'project)
(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))
(add-hook 'project-find-functions #'project-find-go-module)

(defun eglot-format-buffer-on-save ()
  "Format BUFFER on save."
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))
(add-hook 'go-mode-hook #'eglot-format-buffer-on-save)

;;; rustic
;; better support for rust programming than rust-mode
(use-package rustic
  :defer t
  :ensure t
  :config
  (setq rustic-lsp-client 'eglot)
  )

;;; eldoc
;; show function information and possible docstrings
(use-package eldoc
  :diminish
  :hook (after-init . global-eldoc-mode))

;;; yasnippet
;; inserting templated text at given point.
(use-package yasnippet
  :defer t
  :ensure t
  :config
  (setq yas-verbosity 2)
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  ;;(yas-global-mode 1)
  (setq yas-prompt-functions '(yas-dropdown-prompt
                               yas-ido-prompt
                               yas-completing-prompt)))
;;; yasnippet-snippets
;; a set of predefined snippets for many different languages.
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;;; flycheck
;; error checking
(use-package flycheck
  :ensure t
  :config
  (progn
    (global-flycheck-mode)))

;;; go-mode
;; support for the go programming language
(use-package go-mode
  :init
  ;; Format go imports in alphabetical order and also include any missing imports
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  :ensure t)

;;; go-eldoc
;; using eldoc in go
(use-package go-eldoc
  :ensure t)

;;; which-key
;; display available keybindings in a popup
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

;;; markdown-mode
;; support for editing and displaying markdown
(use-package markdown-mode
  :defer t
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;; web-mode
;; basic support for mark-up and programming languages associated with the web
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

;;; typescript-mode
;; support for typescript
(use-package typescript-mode
  :defer t
  :mode "\\.tsx?\\'"
  ;; :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

;;; python-mode
;; support for python
(use-package python-mode
  :defer t
  :ensure t
  ;; :hook (python-mode . lsp-deferred)
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

;;; json-mode
;; support for json
(use-package json-mode
  :defer t
  :ensure t
  :mode (("\\.json\\'" . json-mode))
  :hook (before-save . nro/json-mode-before-save-hook)
  :preface
  (defun nro/json-mode-before-save-hook ()
    (when (eq major-mode 'json-mode)
      (json-pretty-print-buffer))))

;;; vterm
;; a better terminal emulator as it isn't written in elisp :P
(use-package vterm
  :defer t
  :ensure t
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000)
  (defun clean-vterm-window ()
    (hl-line-mode -1))

  :hook (vterm-mode . clean-vterm-window))
(global-set-key (kbd "C-x m") 'vterm)
(use-package vterm-toggle
  :defer t
  :ensure t
  :custom
  (vterm-toggle-scope 'project)
  (vterm-toggle-fullscreen-p nil "Open vterm in another window.")
  :bind(:map vterm-mode-map
             ("s-t" . #'vterm)))

;;; magit
;; git interaction made easy with emacs
(use-package magit
  :ensure t
  :defer t
  :bind (("C-x g" . magit-status)))

;;; smartparens
;; completes parenthesies and other punctuators.
(use-package smartparens
  :defer t
  :ensure t
  :init
  (smartparens-global-mode))

;;; all-the-icons
;; enables usage of many different icons in emacs
(use-package all-the-icons
  :defer t
  :ensure t)

;;; org
;; the classic text editing mode for emacs
(use-package org
  :config
  (setq org-ellipsis "↪")
  (setq org-hide-emphasis-markers t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-agenda-files "~/org/todo.org")

   (setq org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
      (sequence "PLAN(p)" "READY(r)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))
  (define-key global-map (kbd "C-c j")
    (lambda () (interactive) (org-capture nil "jj"))))
(add-hook 'org-mode-hook 'org-indent-mode)
(global-set-key "\C-ca" 'org-agenda)

;;; evil-nerd-commenter
;; support for easily commenting regions in many different languages
;; that have different comment symbols
(use-package evil-nerd-commenter
  :defer t
  :ensure t
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Get the other file in C/C++, i.e. foo.h -> foo.cc | foo.cc -> foo.h
(add-hook 'c-mode-common-hook
  (lambda()
    (local-set-key  (kbd "C-c o") 'ff-find-other-file)))

;;; dired
;; the emacs file manager
(use-package dired
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
  (with-eval-after-load 'dired
    (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)))

(use-package dired-single)
(use-package dired-open
  :config
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("png" . "feh")
                                ("mkv" . "mpv"))))

;; no-littering
;; package to clean-up the ~/.emacs.d folder
(use-package no-littering
  :ensure t
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; fl-todo
;; highlight different different keywords in comments such as
;; TODO: NOTE: ...
(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode)
  :config)

;;; yaml-mode
;; add support for yaml files
(use-package yaml-mode
  :mode ("\\.\\(yml\\|yaml\\|\\config\\|sls\\)$" . yaml-mode)
  :ensure yaml-mode
  :defer t)

;;; emmet-mode
;; help with some css and html stuff to make programming in those languages easier.
(use-package emmet-mode
  :ensure t
  :custom
  (emmet-move-cursor-between-quotes t)
  :custom-face
  (emmet-preview-input ((t (:inherit lazy-highlight))))
  :hook
  (web-mode . emmet-mode)
  (css-mode . emmet-mode))

;;; docker
;; a package to manage docker containers from emacs
(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

;; dockerfile-mode
;; support for dockerfiles, such that they have syntax highlighting
(use-package dockerfile-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

;;; modern-cpp-font-lock
;; better highlighting for fonts in cpp
(use-package modern-cpp-font-lock
  :diminish
  :ensure t
  :init (modern-c++-font-lock-global-mode t))

;;; diminish
;; a package which hides unnecessary minor-modes from the modeline.
(use-package diminish
  :ensure t)

;;; crux
;; a collection of useful keybindings for emacs
(use-package crux
  :ensure t
  :bind (("C-c o" . crux-open-with)))

;;; ini-mode
;; support for .ini files.
(use-package ini-mode
  :defer t
  :ensure t
  :mode "\\.ini\\'")

;;; git-gutter
;; show changed/deleted/created lines in the fringe.
(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode t))

(load "~/.emacs.d/lisp/functions.el")

;; These marking functions are taken from:
;; https://protesilaos.com/codelog/2020-08-03-emacs-custom-functions-galore
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
   "word")

  (nro/mark
   nro/mark-symbol
   "symbol")

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

;;; denote
;; a very simple note taking framework
(use-package denote
  :ensure t
  :defer t
  :config
  (setq denote-directory (expand-file-name "~/notes/"))
  (setq known-keywords '("journal" "papers"))
  (setq denote-file-type nil))
(add-hook 'dired-mode-hook #'denote-dired-mode)

;;; default-text-scale
;; easily change the font size in every buffer.
(use-package default-text-scale
  :defer t
  :ensure t)

;;; clojure-mode
;; support for clojure files
(use-package clojure-mode
  :defer t
  :ensure t)

(setq modus-themes-syntax '(faint))
;; (setq modus-themes-mode-line '(accented 3d borderless))
(load-theme 'modus-vivendi t)

(defun nro/new-journal-entry ()
  "Create an entry tagged 'journal' with the date as its title."
  (interactive)
  (denote
   (format-time-string "%A %e %B %Y")   ; format like Tuesday 14 June 2022
   '("journal")
   nil
   "~/notes/"))

;; Setup for org mode latex
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

;; Load custom variables from a custom.el file, such that they don't clutter up
;; main init.el file.
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
