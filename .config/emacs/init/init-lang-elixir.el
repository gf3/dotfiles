;;; init-lang-elixir.el --- Elixir configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package elixir-mode
  :straight (:host github :repo "elixir-editors/emacs-elixir")
  :mode ("\\.exs?\\'" "\\.heex\\'" "\\.elixir\\'")
  :after tree-sitter)

(use-package eglot-elixir
  :straight (:host github :repo "bvnierop/eglot-elixir")
  :after eglot)

;; (use-package mix
;;   :straight (:host github :repo "ayrat555/mix.el")
;;   :hook ((elixir-mode . mix-minor-mode))
;;   :after elixir-mode)

(provide 'init-lang-elixir)
;;; init-lang-elixir.el ends here
