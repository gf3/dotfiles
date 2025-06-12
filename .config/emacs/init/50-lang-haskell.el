;;; 50-lang-haskell.el --- Haskell configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package haskell-mode
  :straight t
  :hook (haskell-mode . eglot-ensure)
  :custom (haskell-stylish-on-save t))

;; (use-package ormolu
;;   :straight t
;;   :hook (haskell-mode . ormolu-format-on-save-mode)
;;   :bind
;;   (:map haskell-mode-map
;;         ("C-c r" . ormolu-format-buffer)))

(provide '50-lang-haskell)
;;; 50-lang-haskell.el ends here
