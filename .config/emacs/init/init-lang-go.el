;;; init-lang-go.el --- Go configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package go-mode
  :straight t
  :defer t
  :mode ("\\.go\\'")
  :after tree-sitter)

(provide 'init-lang-go)
;;; init-lang-go.el ends here
