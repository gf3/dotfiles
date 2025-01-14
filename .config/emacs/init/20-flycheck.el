;;; init-flycheck.el --- Error checker configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package flycheck
  :straight t
  :init
  (progn
    (define-fringe-bitmap 'my-flycheck-fringe-indicator
      (vector #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00011100
              #b00111110
              #b00111110
              #b00111110
              #b00011100
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000))

    (flycheck-define-error-level 'error
      :severity 2
      :overlay-category 'flycheck-error-overlay
      :fringe-bitmap 'my-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-error)

    (flycheck-define-error-level 'warning
      :severity 1
      :overlay-category 'flycheck-warning-overlay
      :fringe-bitmap 'my-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-warning)

    (flycheck-define-error-level 'info
      :severity 0
      :overlay-category 'flycheck-info-overlay
      :fringe-bitmap 'my-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-info))
  
  (global-flycheck-mode))

(use-package flycheck-color-mode-line
  :straight t
  :hook
  (flycheck-mode . flycheck-color-mode-line-mode)
  :after flycheck)

(use-package flycheck-pos-tip
  :straight t
  :hook
  (flycheck-mode . flycheck-pos-tip-mode)
  :after flycheck)

(provide 'init-flycheck)
;;; init-flycheck.el ends here
