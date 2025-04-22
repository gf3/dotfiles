;;; 20-diffhl.el --- Diff highlighter. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package diff-hl
  :straight t
  :config
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (global-diff-hl-mode))

(provide '20-diffhl)
;;; 20-diffhl.el ends here
