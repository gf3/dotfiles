;;; init-lsp.el --- Language server configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package lsp-mode
  :straight t
  :init
  (setq lsp-keymap-prefix "s-l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (go-mode . lsp-deferred)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :straight t
  :config
  (setq lsp-ui-doc-border "systemYellowColor")
  (setq lsp-ui-doc-border (face-foreground 'default))
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-header t)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-ui-doc-use-webkit t)
  (setq lsp-ui-flycheck-enable t)
  (setq lsp-ui-flycheck-list-position 'right)
  (setq lsp-ui-flycheck-live-reporting t)
  (setq lsp-ui-peek-enable t)
  (setq lsp-ui-sideline-actions-icon lsp-ui-sideline-actions-icon-default)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-ignore-duplicate t)
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-hover t)
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :bind (:map lsp-ui-mode-map
			  ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
			  ([remap xref-find-references] . lsp-ui-peek-find-references))
  :hook (lsp-mode . lsp-ui-mode)
  :commands (lsp-ui-mode)
  :after lsp)

(use-package consult-lsp
  :straight t
  :init
  (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols)
  :after lsp)

(provide 'init-lsp)
;;; init-lsp.el ends here
