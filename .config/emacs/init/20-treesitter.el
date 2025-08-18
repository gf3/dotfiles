;;; 20-treesitter.el --- Tree sitter configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'treesit)

(setq treesit-font-lock-level 6)

(setq major-mode-remap-alist
      '((bash-mode . bash-ts-mode)
        (css-mode . css-ts-mode)
        (elisp-mode . elisp-ts-mode)
        (elixir-mode . elixir-ts-mode)
        (go-mode . go-ts-mode)
        (hcl-mode . hcl-ts-mode)
        (heex-mode . heex-ts-mode)
        (html-mode . html-ts-mode)
        (js2-mode . js-ts-mode)
        (json-mode . json-ts-mode)
        (makefile-mode . make-ts-mode)
        (python-mode . python-ts-mode)
        (toml-mode . toml-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (rust-mode . rust-ts-mode)
        (yaml-mode . yaml-ts-mode)))

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (elixir "https://github.com/elixir-lang/tree-sitter-elixir.git")
        (elm "https://github.com/elm-tooling/tree-sitter-elm")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
        (gren "https://github.com/MaeBrooks/tree-sitter-gren")
        (haskell "https://github.com/tree-sitter/tree-sitter-haskell")
        (hcl "https://github.com/MichaHoffmann/tree-sitter-hcl")
        (heex "https://github.com/phoenixframework/tree-sitter-heex.git")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (liquid "https://github.com/Shopify/tree-sitter-liquid")
        (make "https://github.com/alemuller/tree-sitter-make")
        (nix "https://github.com/nix-community/tree-sitter-nix")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(provide '20-treesitter)
;;; 20-treesitter.el ends here
