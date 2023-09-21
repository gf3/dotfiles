;;; init-asdf.el --- Runtime version manager. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package asdf
  :straight (:host github :repo "tabfugnic/asdf.el")
  :config
  (asdf-enable))

(provide 'init-asdf)
;;; init-asdf.el ends here
