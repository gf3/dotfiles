;;; init-lang-rust.el --- Go configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package rustic
  :straight (:host github :repo "brotzeit/rustic")
  :defer t
  :mode ("\\.rust\\'" "\\.rs\\'")
  :after tree-sitter)

(provide 'init-lang-rust)
;;; init-lang-rust.el ends here
