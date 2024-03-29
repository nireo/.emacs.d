;; init.el --- Personal configuration file -*- lexical-binding: t; -*-

;;; Commentary:
;;; Personal configuration files hosted at https://github.com/nireo/.emacs.d

;;; Code:
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(package-initialize)

;;; use-package
;; a macro to simplify package management in emacs.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name)
(advice-add #'display-startup-echo-area-message :override #'ignore)

(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes/"))

(setq initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

(setq auto-mode-case-fold nil)
;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

(setq-default cursor-in-non-selected-windows nil)
(setq idle-update-delay 1.0)
(setq redisplay-skip-fontification-on-input t)

(dolist (entry '(("/LICENSE\\'" . text-mode)
                 ("\\.log\\'" . text-mode)
                 ("rc\\'" . conf-mode)))
  (push entry auto-mode-alist))

;; Avoid calling menu-bar-mode...etc because to do extra stuff that is not needed.
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(fringe-mode 0)

;; And set these to nil so users don't have to toggle the modes twice to
;; reactivate them.
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)
(setq default-input-method nil)
(setq-default fill-column 80)

;; Disable bidirectional text scanning for a modest performance boost.
;; I've set this to `nil' in the past, but the `bidi-display-reordering's docs
;; say that is an undefined state and suggest this to be just as good:
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Disabling BPA makes redisplay faster, but might produce incorrect
;; reordering of bidirectional text with embedded parentheses (and other
;; bracket characters whose 'paired-bracket' Unicode property is non-nil).
(setq bidi-inhibit-bpa t)

(setq file-name-handler-alist-at-startup file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-at-startup)))

;; Show startup time.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.4fs" (float-time (time-subtract after-init-time before-init-time)))
                     gcs-done)))

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

;; Set custom font
(defvar nro/default-font-size 140)
(defvar nro/default-font "Menlo")
(set-face-attribute 'default nil
                    :family nro/default-font
                    :height nro/default-font-size)

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

;; Shorten answers
(if (boundp 'use-short-answers)
    (setq use-short-answers t))

(global-subword-mode) ;; Make it so that 'w' in evil moves to the next camel case word
(global-auto-revert-mode t) ;; Revert buffers automatically when underlying files are changed externally
(global-display-line-numbers-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace) ;; Delete trailing whitespaces after saving
(global-visual-line-mode 1) ;; Add line wrapping

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Get the other file in C/C++, i.e. foo.h -> foo.cc | foo.cc -> foo.h
(add-hook 'c-mode-common-hook
  (lambda()
    (local-set-key  (kbd "C-c o") 'ff-find-other-file)))

;; Load functions and packages
(load "~/.emacs.d/lisp/packages.el") ;; all packages and their configurations
(load "~/.emacs.d/lisp/functions.el") ;; custom functions
(load "~/.emacs.d/lisp/org.el") ;; org settings

;;(setq modus-themes-syntax 'yellow-comments)
(setq modus-themes-syntax '(alt-syntax))
(setq modus-themes-scale-headings t
      modus-themes-headings ; this is an alist: read the manual or its doc string
      '((1 . (overline background variable-pitch 1.3))
        (2 . (rainbow overline 1.1))
        (t . (semibold))))
(setq modus-themes-completions
      '((matches . (extrabold background intense))
        (selection . (semibold accented intense))
        (popup . (accented))))

;; (setq modus-themes-mode-line '(accented 3d borderless))
(load-theme 'dakrone t)

;; Load custom variables from a custom.el file, such that they don't clutter up
;; main init.el file.
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
