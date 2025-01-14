;;; init-benchmarkinit.el --- Benchmark emacs init. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package benchmark-init
  :straight (:host github :repo "dholm/benchmark-init-el" :branch "master")
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(provide 'init-benchmarkinit)
;;; init-benchmarkinit.el ends here
