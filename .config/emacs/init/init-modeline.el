;;; init-modeline.el --- Mode line configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 32)
  (doom-modeline-hud t)
  (doom-modeline-support-imenu t)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-github t))

(provide 'init-modeline)
;;; init-modeline.el ends here
