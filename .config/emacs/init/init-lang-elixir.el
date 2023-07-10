;;; init-lang-elixir.el --- Elixir configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package elixir-ts-mode
  :straight (:host github :repo "wkirschbaum/elixir-ts-mode")
  :ensure t
  :after eglot
  :config
  (add-to-list 'eglot-server-programs '(elixir-ts-mode "~/.config/emacs/elixir-ls/release/language_server.sh"))
  (add-to-list 'eglot-server-programs '(heex-ts-mode "~/.config/emacs/elixir-ls/release/language_server.sh")))

;;(use-package eglot-elixir
;;  :straight (:host github :repo "hochata/eglot-elixir")
;;  :config
;;  ;; After eglot is loaded
;;  ;; (add-to-list 'eglot-server-programs `(elixir-ts-mode . eglot-elixir))
;;  ;; (add-to-list 'eglot-server-programs `(heex-ts-mode . eglot-elixir))
;;  :after eglot)

(provide 'init-lang-elixir)
;;; init-lang-elixir.el ends here
