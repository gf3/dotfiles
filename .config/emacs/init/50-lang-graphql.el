;;; 50-lang-graphql.el --- GraphQL configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package graphql-mode
  :straight t
  :defer t
  :mode ("\\.graphql\\'")
  :after tree-sitter)

(provide '50-lang-graphql)
;;; 50-lang-graphql.el ends here
