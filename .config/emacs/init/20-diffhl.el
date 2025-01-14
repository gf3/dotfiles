;;; init-diffhl.el --- Diff highlighter. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package diff-hl
  :straight (:host github :repo "dgutov/diff-hl")
  :config
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (global-diff-hl-mode))

(provide 'init-diffhl)
;;; init-diffhl.el ends here
