;;; init-flycheck.el --- Error checker configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package flycheck
  :straight t
  :init (global-flycheck-mode))

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
