;;; 50-lang-elixir.el --- Elixir configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package heex-ts-mode
  :straight (:host github :repo "wkirschbaum/heex-ts-mode")
  :mode "\\.heex\\'")

(use-package elixir-ts-mode
  :straight (:host github :repo "wkirschbaum/elixir-ts-mode")
  :after eglot
  :mode (("\\.ex\\'" . elixir-ts-mode)
         ("\\.exs\\'" . elixir-ts-mode))
  :config
  (global-subword-mode t)
  (add-to-list 'eglot-server-programs '(elixir-ts-mode "~/Code/github.com/elixir-lsp/elixir-ls/_release/language_server.sh"))
  (add-to-list 'eglot-server-programs '(heex-ts-mode "~/Code/github.com/elixir-lsp/elixir-ls/_release/language_server.sh")))

(use-package exunit
  :straight t
  :hook ((elixir-mode . exunit-mode)
         (elixir-ts-mode . exunit-mode))
  :custom
  (transient-default-level 5))

(provide '50-lang-elixir)
;;; 50-lang-elixir.el ends here
