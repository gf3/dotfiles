;;; init-magit.el --- Magit integration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package magit
  :straight (:host github :repo "magit/magit" :branch "master")
  :defer t
  :config
  (setq magit-diff-options '("-b")) ; ignore whitespace
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq magit-define-global-key-bindings t)
  (transient-suffix-put 'magit-dispatch "k" :key "x"))

(provide 'init-magit)
;;; init-magit.el ends here
