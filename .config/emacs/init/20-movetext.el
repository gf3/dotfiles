;; 20-movetext.el --- Move text around. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package move-text
  :straight t
  :demand t
  :commands (move-text-up move-text-down)
  :config
  (defun indent-region-advice (&rest ignored)
    (let ((deactivate deactivate-mark))
      (if (region-active-p)
          (indent-region (region-beginning) (region-end))
        (indent-region (line-beginning-position) (line-end-position)))
      (setq deactivate-mark deactivate)))

  (advice-add 'move-text-up :after 'indent-region-advice)
  (advice-add 'move-text-down :after 'indent-region-advice)
  
  (move-text-default-bindings))

(provide '20-movetext)
;;; 20-movetext.el ends here
