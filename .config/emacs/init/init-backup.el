;;; init-backup.el --- Backup configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Backup
(setq backup-directory-alist
	  `((".*" . ,(concat user-emacs-directory (file-name-as-directory "tmp") (file-name-as-directory "backups")))))
(setq version-control t)
(setq backup-by-copying t)
(setq delete-old-versions t)

;; Autosave
(setq auto-save-file-name-transforms
	  `((".*" ,(concat user-emacs-directory (file-name-as-directory "tmp") (file-name-as-directory "autosaves")) t)))
(setq auto-save-default t)
(setq auto-save-timeout 10)
(setq auto-save-interval 200)

(provide 'init-backup)
;;; init-backup.el ends here
