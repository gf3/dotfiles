;;; 20-alltheicons.el --- All the icons configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package all-the-icons
  :straight t
  :if (display-graphic-p))

(use-package all-the-icons-completion
  :straight t
  :after all-the-icons
  :config
  (all-the-icons-completion-mode))

(provide '20-alltheicons)
;;; 20-alltheicons.el ends here
