;;; 50-lang-purescript.el --- Purescript configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package purescript-mode
  :straight t
  :config
  (defun myhook-purescript-mode ()
    (turn-on-purescript-indentation)
    (add-hook 'before-save-hook #'purescript-sort-imports nil t))
  (add-hook 'purescript-mode-hook #'myhook-purescript-mode)
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(purescript-mode  . ("purescript-language-server" "--stdio"
                                       "--config {\"purescript.addNpmPath\": true,
                                              \"purescript.buildCommand\": \"npx spago build --purs-args --json-errors\"}" )))))

(use-package psc-ide
  :straight t
  :config
  (setq psc-ide-use-npm-bin t)
  (add-hook 'purescript-mode-hook
            (lambda ()
              (psc-ide-mode))))

(use-package psci
  :straight t
  :config
  (add-hook 'purescript-mode-hook 'inferior-psci-mode))

(provide '50-lang-purescript)
;;; 50-lang-purescript.el ends here
