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

(use-package exunit
  :straight (:host github :repo "ananthakumaran/exunit.el")
  :ensure t
  :hook ((elixir-mode . exunit-mode)
         (elixir-ts-mode . exunit-mode)))

(use-package inf-elixir
  :straight (:host github :repo "J3RN/inf-elixir")
  :bind (("C-c i i" . 'inf-elixir)
         ("C-c i p" . 'inf-elixir-project)
         ("C-c i l" . 'inf-elixir-send-line)
         ("C-c i r" . 'inf-elixir-send-region)
         ("C-c i b" . 'inf-elixir-send-buffer)
         ("C-c i R" . 'inf-elixir-reload-module)))

(provide 'init-lang-elixir)
;;; init-lang-elixir.el ends here
