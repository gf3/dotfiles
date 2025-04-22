;;; 50-lang-yaml.el --- YAML configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package yaml-mode
  :straight t
  :defer t
  :mode ("\\.yaml\\'" "\\.yml\\'")
  :hook ((yaml-mode . (lambda ()
						            (define-key yaml-mode-map "\C-m" 'newline-and-indent)))))

(provide '50-lang-yaml)
;;; 50-lang-yaml.el ends here
