;;; init-treesitter.el --- Tree sitter configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'treesit)

(add-to-list 'major-mode-remap-alist
			 '(elixir-mode . elixir-ts-mode))

(add-to-list
 'treesit-language-source-alist
 '(typescript "https://github.com/tree-sitter/tree-sitter-typescript.git"))

(add-to-list
 'treesit-language-source-alist
 '(heex "https://github.com/phoenixframework/tree-sitter-heex.git"))

(add-to-list
 'treesit-language-source-alist
 '(elixir "https://github.com/elixir-lang/tree-sitter-elixir.git"))

(use-package heex-ts-mode
  :straight (:host github :repo "wkirschbaum/heex-ts-mode"))

(use-package elixir-ts-mode
  :straight (:host github :repo "wkirschbaum/elixir-ts-mode")
  :config
    (global-subword-mode t))

(provide 'init-treesitter)
;;; init-treesitter.el ends here
