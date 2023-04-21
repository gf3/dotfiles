;;; init-writeroom.el --- Distraction-free writing. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package writeroom-mode
  :straight (:host github :repo "joostkremers/writeroom-mode")
  :defer t
  :config
  (setq writeroom-fullscreen-effect 'maximized))

(provide 'init-writeroom)
;;; init-writeroom.el ends here
