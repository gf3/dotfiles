;;; init-lang-go.el --- Go configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun gf3/go-mode-hook ()
  "Go mode hook."
  ;; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet")))

(add-hook 'go-mode-hook 'gf3/go-mode-hook)
(add-hook 'go-ts-mode-hook 'gf3/go-mode-hook)

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
			 go-run))

(use-package go-gen-test
  :straight (:host github :repo "s-kostyaev/go-gen-test")
  :defer t
  :commands (go-gen-test-dwim
             go-gen-test-all
             go-gen-test-exported))

(use-package go-dlv
  :straight (:host github :repo "benma/go-dlv.el")
  :defer t)

(provide 'init-lang-go)
;;; init-lang-go.el ends here
