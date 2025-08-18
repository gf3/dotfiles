;;; 19-eglot.el --- Language server configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package eglot
  :straight t
  :preface
  (defun mp-eglot-eldoc ()
    (setq eldoc-documentation-strategy
          'eldoc-documentation-compose-eagerly))
  :hook ((eglot-managed-mode . mp-eglot-eldoc))
  :config
  (setq eldoc-echo-area-use-multiline-p t)
  (setq eglot-confirm-server-initiated-edits nil)
  (setq eglot-extend-to-xref t))

(use-package consult-eglot
  :straight t
  :after eglot
  :commands (consult-eglot-symbols))

(provide '19-eglot)
;;; 19-eglot.el ends here
