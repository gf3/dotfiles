;;; 50-lang-zig.el --- Odin configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package zig-mode
  :straight t
  :mode ("\\.zig\\'")
  :after tree-sitter)

(if (>= emacs-major-version 28)
    (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
  (progn
    (defun colorize-compilation-buffer ()
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region compilation-filter-start (point))))
    (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)))

;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                '(zig-mode . ("/Users/gianni/Code/ols/ols"))))

(provide '50-lang-zig)
;;; 50-lang-zig.el ends here
