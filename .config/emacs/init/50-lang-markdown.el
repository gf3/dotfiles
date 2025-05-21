;;; 50-lang-markdown.el --- Markdown configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package markdown-mode
  :straight t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

(provide '50-lang-markdown)
;;; 50-lang-markdown.el ends here
