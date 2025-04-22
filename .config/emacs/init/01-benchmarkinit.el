;;; 20-benchmarkinit.el --- Benchmark emacs init. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package benchmark-init
  :straight t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(provide '20-benchmarkinit)
;;; 20-benchmarkinit.el ends here
