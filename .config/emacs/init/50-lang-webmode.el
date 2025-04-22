;;; 50-lang-webmode.el --- Web mode configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package web-mode
  :straight t
  :mode (("\\.html\\'" . web-mode)
         ("\\.liquid\\'" . web-mode))
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

(provide '50-lang-webmode)
;;; 50-lang-webmode.el ends here
