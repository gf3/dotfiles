;;; 20-deadgrep.el --- Initialize deadgrep         -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package deadgrep
  :straight t
  :demand t
  :bind (("C-c g" . deadgrep)))

(provide '20-deadgrep)
;;; 20-deadgrep.el ends here

