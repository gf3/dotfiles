;;; init-treesitter.el --- Tree sitter configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'treesit)

(setq major-mode-remap-alist
      '((bash-mode . bash-ts-mode)
        (css-mode . css-ts-mode)
        (elisp-mode . elisp-ts-mode)
        (elixir-mode . elixir-ts-mode)
        (go-mode . go-ts-mode)
        (heex-mode . heex-ts-mode)
        (html-mode . html-ts-mode)
        (js2-mode . js-ts-mode)
        (json-mode . json-ts-mode)
        (makefile-mode . make-ts-mode)
        (markdown-mode . markdown-ts-mode)
        (python-mode . python-ts-mode)
        (toml-mode . toml-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (yaml-mode . yaml-ts-mode)))

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (elixir "https://github.com/elixir-lang/tree-sitter-elixir.git")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
        (heex "https://github.com/phoenixframework/tree-sitter-heex.git")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(use-package heex-ts-mode
  :straight (:host github :repo "wkirschbaum/heex-ts-mode"))

(use-package elixir-ts-mode
  :straight (:host github :repo "wkirschbaum/elixir-ts-mode")
  :config
  (global-subword-mode t))

(provide 'init-treesitter)
;;; init-treesitter.el ends here
