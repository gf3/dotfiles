;;; init-helpful.el --- Better help system. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package helpful
  :straight (:host github :repo "Wilfred/helpful")
  :bind (("C-h f" . helpful-callable)
		 ("C-h v" . helpful-variable)
		 ("C-h k" . helpful-key)
		 ("C-h F" . helpful-function)
		 ("C-h C" . helpful-command)
		 ("C-c C-d" . helpful-at-point)))

(provide 'init-helpful)
;;; init-helpful.el ends here
