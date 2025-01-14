;;; init-psession.el --- Session managmement. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package psession
  :straight (:host github :repo "thierryvolpiatto/psession")
  :config
  (psession-mode 1)
  (psession-savehist-mode 1)
  (psession-autosave-mode 1))

(provide 'init-psession)
;;; init-psession.el ends here
