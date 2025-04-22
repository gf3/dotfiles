;;; 50-lang-go.el --- Go configuration. -*- lexical-binding: t -*-
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

(use-package gotest
  :straight t
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
  :straight t
  :defer t
  :mode ("\\.go\\'")
  :commands (go-gen-test-dwim
             go-gen-test-all
             go-gen-test-exported))

(use-package go-dlv
  :straight t
  :mode ("\\.go\\'")
  :defer t)

(provide '50-lang-go)
;;; 50-lang-go.el ends here
