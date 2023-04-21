;;; init-embrace.el --- Quickly edit surrounding characters. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package embrace
  :straight (:type git :host github :repo "cute-jumper/embrace.el")
  :bind (("C-M-s-#" . embrace-commander))
  :config
  (add-hook 'org-mode-hook 'embrace-org-mode-hook)
  (defun embrace-markdown-mode-hook ()
    (dolist (lst '((?* "*" . "*")
                   (?\ "\\" . "\\")
                   (?$ "$" . "$")
                   (?/ "/" . "/")))
      (embrace-add-pair (car lst) (cadr lst) (cddr lst))))
  (add-hook 'markdown-mode-hook 'embrace-markdown-mode-hook))

(provide 'init-embrace)
;;; init-embrace.el ends here
