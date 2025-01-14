;;; init-popwin.el --- Fix popup windows. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package popwin
  :straight t
  :config
  ;; M-x compile
  (push '(compilation-mode :noselect t) popwin:special-display-config)

  ;; vc
  (push "*vc-diff*" popwin:special-display-config)
  (push "*vc-change-log*" popwin:special-display-config)

  ;; undo-tree
  (push '(" *undo-tree*" :width 0.3 :position right) popwin:special-display-config)

  ;; Warnings
  (push '("*Warnings*" :noselect t) popwin:special-display-config)
  :init
  (popwin-mode 1))

(provide 'init-popwin)
;;; init-popwin.el ends here
