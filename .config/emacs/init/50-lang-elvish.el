;;; init-lang-elvish.el --- Elvish configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package elvish-mode
  :straight (:host github :repo "ALSchwalm/elvish-mode")
  :defer t
  :mode ("\\.elv\\'"))

(provide 'init-lang-elvish)
;;; init-lang-elvish.el ends here
