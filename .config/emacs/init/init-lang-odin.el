;;; init-lang-odin.el --- Odin configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package odin-mode
  :straight (:host github :repo "mattt-b/odin-mode")
  :mode ("\\.odin\\'")
  :after tree-sitter)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(odin-mode . ("/Users/gianni/Code/ols/ols"))))

(provide 'init-lang-odin)
;;; init-lang-odin.el ends here
