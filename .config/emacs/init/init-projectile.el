;;; init-projectile.el --- Project management configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package projectile
  :straight t
  :init
  (setq projectile-enable-caching t)
  (setq projectile-sort-order 'recently-active)
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(provide 'init-projectile)
;;; init-projectile.el ends here
