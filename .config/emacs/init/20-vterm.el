;;; init-vterm.el --- VTerm configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package vterm
  :straight (:host github :repo "akermu/emacs-libvterm")
  :bind ("s-!" . vterm)
  :commands (vterm))

(provide 'init-vterm)
;;; init-vterm.el ends here
