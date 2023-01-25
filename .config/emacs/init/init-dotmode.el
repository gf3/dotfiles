;;; init-dotmode.el --- Vim-like normal/insert repeat with `.'. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package dot-mode
  :straight (:host github :repo "wyrickre/dot-mode")
  :config
  (global-dot-mode t))

(provide 'init-dotmode)
;;; init-dotmode.el ends here
