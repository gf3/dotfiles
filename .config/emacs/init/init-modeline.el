;;; init-modeline.el --- Mode line configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 32)
  (setq doom-modeline-hud t))

(provide 'init-modeline)
;;; init-modeline.el ends here
