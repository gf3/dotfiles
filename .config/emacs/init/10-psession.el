;;; 20-psession.el --- Session managmement. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package psession
  :straight t
  :config
  (psession-mode 1)
  (psession-savehist-mode 1)
  (psession-autosave-mode 1))

(provide '20-psession)
;;; 20-psession.el ends here
