;;; init-lang-typescript.el --- Typescript configuration. -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:

(use-package typescript-mode
  :straight (:host github :repo "emacs-typescript/typescript.el" :branch "master")
  :defer t
  :mode ("\\.ts\\'" "\\.tsx\\'")
  :after tree-sitter)

(provide 'init-lang-typescript)
;;; init-lang-typescript.el ends here
