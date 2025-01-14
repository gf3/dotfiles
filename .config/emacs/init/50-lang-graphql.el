;;; init-lang-graphql.el --- GraphQL configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package graphql-mode
  :straight (:host github :repo "davazp/graphql-mode" :branch "master")
  :defer t
  :mode ("\\.graphql\\'")
  :after tree-sitter)

(provide 'init-lang-graphql)
;;; init-lang-graphql.el ends here
