;;; init-lang-go.el --- Go configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package go-mode
  :straight t
  :defer t
  :mode ("\\.go\\'")
  :after tree-sitter)

(use-package gotest.el
  :straight (:host github :repo "nlamirault/gotest.el")
  :commands (go-test-current-test
			 go-test-current-test-cache
			 go-test-current-file
			 go-test-current-project
			 go-test-current-coverage
			 go-test-current-benchmark
			 go-test-current-file-benchmarks
			 go-test-current-project-benchmarks
			 go-run)
  :after go-mode)

(provide 'init-lang-go)
;;; init-lang-go.el ends here
