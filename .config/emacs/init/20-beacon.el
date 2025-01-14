;;; init-beacon.el --- Highlight the current line. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package beacon
  :straight (:host github :repo "Malabarba/beacon")
  :config
  (beacon-mode 1))

(provide 'init-beacon)
;;; init-beacon.el ends here
