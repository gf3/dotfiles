;;; init-lang-typescript.el --- Typescript configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package js
  :straight t
  :defer t
  :custom
  (js-indent-level 2))

(use-package typescript-mode
  :straight (:host github :repo "emacs-typescript/typescript.el" :branch "master")
  :defer t
  :mode ("\\.ts\\'" "\\.tsx\\'")
  :custom
  (typescript-indent-level 2)
  :config
  (put 'typescript-mode 'eglot-language-id "typescript.tsx")
  (put 'typescript-ts-mode 'eglot-language-id "typescript.tsx"))

(provide 'init-lang-typescript)
;;; init-lang-typescript.el ends here
