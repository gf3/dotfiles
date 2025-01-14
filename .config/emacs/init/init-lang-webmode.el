;;; init-lang-webmode.el --- Web mode configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package web-mode
  :straight (:host github :repo "fxbois/web-mode" :branch "master")
  :after (projectile)
  :defer t
  :mode ("\\.html\\'")
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-current-element-highlight nil)
  (setq web-mode-enable-current-column-highlight nil)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-block-face t)
  (setq web-mode-enable-part-face t)
  (setq web-mode-enable-comment-interpolation t)
  (add-to-list 'auto-mode-alist
			   '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist
			   '("\\.liquid\\'" . web-mode))
  (setq web-mode-engines-alist
		'(("go" . "\\.html\\'"))))

(provide 'init-lang-webmode)
;;; init-lang-webmode.el ends here
