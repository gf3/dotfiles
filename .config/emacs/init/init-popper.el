;;; init-popper.el --- Tame popup windows. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package popper
  :straight t
  :ensure t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*Warnings\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints

(provide 'init-popper)
;;; init-popper.el ends here
