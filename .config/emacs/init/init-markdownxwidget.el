;;; init-markdownxwidget.el --- Markdown previews. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package markdown-xwidget
  :after markdown-mode
  :straight (markdown-xwidget
             :type git
             :host github
             :repo "cfclrk/markdown-xwidget"
             :files (:defaults "resources"))
  :bind (:map markdown-mode-command-map
              ("x" . markdown-xwidget-preview-mode))
  :custom
  (markdown-xwidget-command "multimarkdown")
  (markdown-xwidget-github-theme "light")
  (markdown-xwidget-mermaid-theme "default")
  (markdown-xwidget-code-block-theme "default"))

(provide 'init-markdownxwidget)
;;; init-markdownxwidget.el ends here
