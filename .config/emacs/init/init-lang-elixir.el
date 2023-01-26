;;; init-lang-elixir.el --- Elixir configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package eglot-elixir
  :straight (:host github :repo "hochata/eglot-elixir")
  :config
  ;; After eglot is loaded
  (add-to-list 'eglot-server-programs `(elixir-ts-mode . eglot-elixir))
  (add-to-list 'eglot-server-programs `(heex-ts-mode . eglot-elixir))
  :after eglot)

(provide 'init-lang-elixir)
;;; init-lang-elixir.el ends here
