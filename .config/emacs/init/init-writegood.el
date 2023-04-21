;;; init-writegood.el --- Find common writing problems. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package writegood-mode
  :straight (:host github :repo "bnbeckwith/writegood-mode")
  :defer t)

(defalias 'gf3/writing
  (let ((map (make-sparse-keymap)))
	(define-key map (kbd "a") #'writegood-grade-level)
	(define-key map (kbd "e") #'writegood-reading-ease)
	map)
  "Writing tools.")

(global-set-key (kbd "C-c w") 'gf3/writing)

(provide 'init-writegood)
;;; init-writegood.el ends here
