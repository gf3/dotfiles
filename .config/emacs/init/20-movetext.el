;; init-movetext.el --- Move text around. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package move-text
  :straight (:host github :repo "emacsfodder/move-text")
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

(provide 'init-movetext)
;;; init-movetext.el ends here
