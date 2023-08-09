;;; init-lang-docker.el --- Control docker. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package docker
  :straight (:host github :repo "Silex/docker.el")
  :bind ("C-c d" . docker))

(if (boundp 'dockerfile-ts-mode)
    (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-ts-mode)))

(provide 'init-lang-docker)
;;; init-lang-docker.el ends here
