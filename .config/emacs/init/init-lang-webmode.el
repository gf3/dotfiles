;;; init-lang-webmode.el --- Web mode configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun gf3/web-mode-erb-hook nil
  "Personal erb settings."
  (if (projectile-project-p)
	  (cond ((file-exists-p (concat (projectile-project-root) "Gemfile"))
			 (progn
			   (setq web-mode-markup-indent-offset 2)
			   (web-mode-set-engine "erb")))
			((file-exists-p (concat (projectile-project-root) "mix.exs"))
			 (progn
			   (setq-local apheleia-formatter 'mix-filename-format))))))

(use-package web-mode
  :straight (:host github :repo "fxbois/web-mode" :branch "master")
  :after (projectile)
  :defer t
  :mode ("\\.html\\'" "\\.erb\\'")
  :hook (web-mode . gf3/web-mode-erb-hook)
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
  (add-to-list 'auto-mode-alist
			   '("\\.erb\\'" . web-mode))
  (setq web-mode-engines-alist
		'(("go" . "\\.html\\'")
		  ("erb" . "\\.html\\.erb\\'"))))

(provide 'init-lang-webmode)
;;; init-lang-webmode.el ends here
