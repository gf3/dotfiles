;;; 50-lang-fsharp.el --- F# configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package fsharp-mode
  :straight t
  :defer t
  :ensure t
  :config
  (remove-hook 'project-find-functions #'fsharp-mode-project-root))


(use-package eglot-fsharp
  :straight t
  :after fsharp-mode
  :hook
  ((fsharp-mode . eglot-ensure)))

(provide '50-lang-fsharp)
;;; 50-lang-fsharp.el ends here
