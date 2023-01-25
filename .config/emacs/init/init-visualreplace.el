;;; init-visualreplace.el --- Visual search and replace. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package visual-replace
  :straight (:host github :repo "szermatt/visual-replace")
  :bind (("C-c r" . visual-replace)
         :map isearch-mode-map
         ("C-c r" . visual-replace-from-isearch))
  :config
  (visual-replace-global-mode 1))

(provide 'init-visualreplace)
;;; init-visualreplace.el ends here
