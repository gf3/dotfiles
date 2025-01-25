;;; init-lang-elixir.el --- Elixir configuration. -*- lexical-binding: t -*-
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
  :straight (:host github :repo "ananthakumaran/exunit.el")
  :hook ((elixir-mode . exunit-mode)
         (elixir-ts-mode . exunit-mode)))

(provide 'init-lang-elixir)
;;; init-lang-elixir.el ends here
