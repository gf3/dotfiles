;;; init-deadgrep.el --- Initialize deadgrep         -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package deadgrep
  :straight (:host github :repo "Wilfred/deadgrep")
  :ensure t
  :bind (("C-c g" . deadgrep)))

(provide 'init-deadgrep)
;;; init-deadgrep.el ends here

