;;; init-lang-docker.el --- Control docker. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package docker
  :straight (:host github :repo "Silex/docker.el")
  :bind ("C-c d" . docker))

(provide 'init-lang-docker)
;;; init-lang-docker.el ends here
