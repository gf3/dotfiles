;;; 50-lang-elixir.el --- Elixir configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package heex-ts-mode
  :straight t
  :mode "\\.heex\\'")

(use-package elixir-ts-mode
  :straight t
  :mode (("\\.ex\\'" . elixir-ts-mode)
         ("\\.exs\\'" . elixir-ts-mode))
  :config
  (global-subword-mode t)
  (with-eval-after-load 'eglot
    (add-hook 'elixir-ts-mode-hook 'eglot-ensure)
    (add-hook 'heex-ts-mode-hook 'eglot-ensure)
    (add-to-list 'eglot-server-programs
                 '(elixir-mode . ("elixir-ls")))
    (add-to-list 'eglot-server-programs
                 '(elixir-ts-mode . ("elixir-ls")))
    (add-to-list 'eglot-server-programs
                 '(heex-mode . ("elixir-ls")))
    (add-to-list 'eglot-server-programs
                 '(heex-ts-mode . ("elixir-ls")))))

(use-package exunit
  :straight t
  :hook ((elixir-mode . exunit-mode)
         (elixir-ts-mode . exunit-mode))
  :custom
  (transient-default-level 5))

(provide '50-lang-elixir)
;;; 50-lang-elixir.el ends here
