;;; 50-lang-docker.el --- Control docker. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package docker
  :straight t
  :bind ("C-c d" . docker))

(if (boundp 'dockerfile-ts-mode)
    (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-ts-mode)))

(provide '50-lang-docker)
;;; 50-lang-docker.el ends here
