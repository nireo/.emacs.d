;; packages.el --- Packages belonging to my personal configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; List of Emacs packages for my configuration.

;;; Code:

;; For esup
(define-advice load-file (:override (file) silence)
  (load file nil 'nomessage))

(defvar profile-dotemacs-file "~/.emacs.d" "File to be profiled.")
(define-advice startup--load-user-init-file (:before (&rest _) undo-silence)
      (advice-remove #'load-file #'load-file@silence))

;;; esup
;; tool to benchmark emacs start-up time.
(use-package esup
  :ensure t
  :config
  (setq esup-depth 0))

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

  (setq evil-insert-state-cursor 'hbar)

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
  :custom
  (vertico-count 14)
  (vertico-multiform-categories
   '((file flat)))
  :init
  (vertico-mode)
  (vertico-multiform-mode))

(advice-add #'vertico--format-candidate :around
            (lambda (orig cand prefix suffix index _start)
              (setq cand (funcall orig cand prefix suffix index _start))
              (concat
               (if (= vertico--index index)
                   (propertize "> " 'face 'vertico-current)
                 "  ")
               cand)))

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
    (add-to-list 'projectile-globally-ignored-files "_cache")))

;;; eglot
;; simple language server interface for emacs. (simpler than lsp-mode)
(use-package eglot
  :ensure t
  :hook ((c-mode c++-mode go-mode rustic-mode python-mode) . eglot-ensure)
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

;; format buffers when emacs saves them.
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
  (setq rustic-lsp-client 'eglot))

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

;;; evil-nerd-commenter
;; support for easily commenting regions in many different languages
;; that have different comment symbols
(use-package evil-nerd-commenter
  :defer t
  :ensure t
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

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

;; hl-todo
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

;;; blacken
;; format python files with black
(use-package blacken
  :ensure t
  :defer t
  :custom
  (blacken-allow-py36 t)
  (blacken-skip-string-normalization t)
  :hook (python-mode-hook . blacken-mode))

;;; writeroom-mode
;; nice focused writing mode
(use-package writeroom-mode
  :ensure t)

;;; corfu
;; completion package as an alternative for company. a lot simpler
;; and faster in my experience. looks nicer as well :P
(use-package corfu
  :ensure t
  :init
  (setq corfu-auto t
        corfu-auto-delay 0
        corfu-auto-prefix 0
        completion-styles '(basic))
  (global-corfu-mode))

(define-key corfu-map (kbd "M-n") #'corfu-doc-scroll-up)  ;; corfu-previous
(define-key corfu-map (kbd "M-p") #'corfu-doc-scroll-down) ;; corfu-next
(add-hook 'corfu-mode-hook #'corfu-doc-mode)

(use-package corfu-doc
  :ensure t)

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(with-eval-after-load 'eglot
   (setq completion-category-defaults nil))

;;; packages.el ends here
