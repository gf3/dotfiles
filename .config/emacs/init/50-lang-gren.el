;;; 50-lang-gren.el --- Gren configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package gren-mode
  :straight (:type git :host github :repo "MaeBrooks/gren-mode")
  :mode (("\\.gren\\'" . gren-ts-mode)))

(provide '50-lang-gren)
;;; 50-lang-gren.el ends here
