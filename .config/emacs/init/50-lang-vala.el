;;; 50-lang-vala.el --- Vala configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package vala-mode
  :straight t
  :after eglot
  :mode (("\\.vala\\'" . vala-mode))
  :config
  (add-to-list 'eglot-server-programs '(vala-mode "vala-language-server")))

(provide '50-lang-vala)
;;; 50-lang-vala.el ends here
