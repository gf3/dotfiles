;;; init-eglot.el --- Language server configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package eglot
  :straight (:host github :repo "joaotavora/eglot")
  :hook ((elixir-ts-mode . eglot-ensure)
         (go-ts-mode . eglot-ensure)
         (hcl-ts-mode . eglot-ensure)
         (heex-ts-mode . eglot-ensure)
         (zig-mode . eglot-ensure))
  :config
  (setq eldoc-echo-area-use-multiline-p t)
  (setq eglot-confirm-server-initiated-edits nil)
  (setq eglot-extend-to-xref t)
  (when (file-directory-p (expand-file-name "~/Code/github.com/elixir-tools/next-ls/bin"))
    (add-to-list 'exec-path (expand-file-name "~/Code/github.com/elixir-tools/next-ls/bin"))
    (add-to-list 'eglot-server-programs
                 `((elixir-ts-mode heex-ts-mode elixir-mode) . ("nextls" "--stdio=true")))))

(use-package eldoc-box
  :straight (:host github :repo "casouri/eldoc-box")
  :hook (eglot-managed-mode . eldoc-box-hover-at-point-mode)
  :after eglot)

(use-package consult-eglot
  :straight (:host github :repo "mohkale/consult-eglot")
  :after eglot
  :commands (consult-eglot-symbols))

(defalias 'gf3/eglot
  (let ((map (make-sparse-keymap)))
	(define-key map (kbd "a") #'eglot-code-actions)
	(define-key map (kbd "f") #'eglot-format)
	(define-key map (kbd "r") #'eglot-rename)
	map)
  "Eglot commands.")

(global-set-key (kbd "C-c c") 'gf3/eglot)

(provide 'init-eglot)
;;; init-eglot.el ends here
