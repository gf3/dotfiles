;;; 10-session.el --- Session managmement. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Restore buffers
(setq desktop-save t)
(setq desktop-dirname (file-name-concat user-emacs-directory "tmp"))
(setq desktop-base-file-name "emacs.desktop")
(setq desktop-load-locked-desktop t)
(setq desktop-restore-frames t)
(setq desktop-auto-save-timeout 300)

(desktop-save-mode 1)

;; Restore cursor position
(setq save-place-file (file-name-concat user-emacs-directory "tmp" "places"))

(save-place-mode 1)

;; Restore history
(setq savehist-file (file-name-concat user-emacs-directory "tmp" "history"))
(setq savehist-additional-variables '(command-history vertico-repeat-history kill-ring search-ring regexp-search-ring compile-history log-edit-comment-ring))

(savehist-mode 1)

(provide '10-session)
;;; 10-session.el ends here
