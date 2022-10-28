;; mode-line.el --- Configuration for the modeline in my config -*- lexical-binding: t; -*-


;;; Commentary:
;;; Setting for my mode-line

;;; Code:

(setq-default mode-line-format
      (list
       '(:eval
         (list
          (propertize "%b" help-echo (buffer-file-name))))
       ))

;;; mode-line.el ends here
