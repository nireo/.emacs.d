;; -*- lexical-binding: t; -*-

;;; Code:

;; Defer garbage collection further back in the startup process.
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.5)

(setq frame-inhibit-implied-resize t)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

(setq-default file-name-handler-alist nil)

;; From doom emacs.
(let ((old-file-name-handler-alist file-name-handler-alist))
    (setq-default file-name-handler-alist nil)
    (defun nro-reset-file-handler-alist-h ()
      (setq file-name-handler-alist
            (delete-dups (append file-name-handler-alist
                                 old-file-name-handler-alist))))
    (add-hook 'emacs-startup-hook #'nro-reset-file-handler-alist-h 101))

(setq-default inhibit-redisplay t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil)
            (redisplay)))

(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

(setq default-input-method nil)

;;; early-init.el ends here
