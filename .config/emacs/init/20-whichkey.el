;;; init-whichkey.el --- Show available commands. -*- lexical-binding: t -*-

(use-package which-key
  :straight t
  :demand t
  :config
  (which-key-mode)
  (push '((nil . "ryo:.*:") . (nil . "")) which-key-replacement-alist)
  (which-key-setup-side-window-bottom)
  (setq which-key-idle-delay 0.5)
  (setq which-key-idle-secondary-delay 0.05)
  (setq which-key-show-early-on-C-h t)
  (setq which-key-max-description-length 50)
  (setq which-key-max-display-columns nil)
  (setq which-key-side-window-max-width 0.33)
  (setq which-key-side-window-max-height 0.25)
  (setq which-key-side-window-location 'bottom)
  (setq which-key-sort-order 'which-key-key-order-alpha)
  (setq which-key-side-window-slot -10)
  (setq which-key-popup-type 'side-window)
  (setq which-key-separator " → "))

(provide 'init-whichkey)

;;; init-whichkey.el ends here
