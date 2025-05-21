;;; 50-lang-haskell.el --- Haskell configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package haskell-mode
  :straight t
  :hook (haskell-mode . eglot-ensure))

(provide '50-lang-haskell)
;;; 50-lang-haskell.el ends here
