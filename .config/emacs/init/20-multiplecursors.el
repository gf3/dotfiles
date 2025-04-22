;;; 20-multiplecursors.el --- Miultiple cursors configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package multiple-cursors
  :straight t
  :custom
  (mc/always-repeat-command t)
  (mc/always-run-for-all t)
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)))

(provide '20-multiplecursors)
;;; 20-multiplecursors.el ends here
