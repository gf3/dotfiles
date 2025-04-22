;;; 50-lang-rust.el --- Go configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package rust-mode
  :straight t
  :custom
  (rust-format-on-save t)
  (rust-mode-treesitter-derive t))

(use-package cargo-mode
  :straight t
  :hook
  (rust-mode . cargo-minor-mode)
  :config
  (setq compilation-scroll-output t))

(provide '50-lang-rust)
;;; 50-lang-rust.el ends here
