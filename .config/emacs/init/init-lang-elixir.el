;;; init-lang-elixir.el --- Elixir configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package heex-ts-mode
  :straight (:host github :repo "wkirschbaum/heex-ts-mode")
  :ensure t)

(use-package elixir-ts-mode
  :straight (:host github :repo "wkirschbaum/elixir-ts-mode")
  :after eglot
  :ensure t
  :config
  (global-subword-mode t)
  (add-to-list 'eglot-server-programs '(elixir-ts-mode "~/Code/github.com/elixir-lsp/elixir-ls/bin/language_server.sh"))
  (add-to-list 'eglot-server-programs '(heex-ts-mode "~/Code/github.com/elixir-lsp/elixir-ls/bin/language_server.sh")))

(provide 'init-lang-elixir)
;;; init-lang-elixir.el ends here
