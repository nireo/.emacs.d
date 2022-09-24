;; -*- lexical-binding: t; -*-

;;; Code:

;; Defer garbage collection further back in the startup process.
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.5)

(setq frame-inhibit-implied-resize t)

(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

(setq-default inhibit-redisplay t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil)
            (redisplay)))

(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")

(setq default-input-method nil)
(setq frame-inhibit-implied-resize t)
(setq inhibit-x-resources t)
(setq inhibit-startup-message t) ;; Remove startup message

;;; early-init.el ends here
