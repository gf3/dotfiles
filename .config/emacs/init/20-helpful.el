;;; 20-helpful.el --- Better help system. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package helpful
  :straight t
  :bind (("C-h f" . helpful-callable)
		     ("C-h v" . helpful-variable)
		     ("C-h k" . helpful-key)
		     ("C-h F" . helpful-function)
		     ("C-h C" . helpful-command)
		     ("C-c C-d" . helpful-at-point)))

(provide '20-helpful)
;;; 20-helpful.el ends here
