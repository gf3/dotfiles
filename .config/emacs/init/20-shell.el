;;; 20-shell.el --- Shell configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun gf3/term-custom-settings ()
  (local-set-key (kbd "M-p") 'term-send-up)
  (local-set-key (kbd "M-n") 'term-send-down)
  (define-key term-raw-map (kbd "M-o") 'other-window)
  (define-key term-raw-map (kbd "M-p") 'term-send-up)
  (define-key term-raw-map (kbd "M-n") 'term-send-down))

(add-hook 'term-load-hook #'gf3/term-custom-settings)

;; Directory tracking
(add-hook 'comint-output-filter-functions #'comint-osc-process-output)

;; Clear the screen
(add-hook 'eshell-mode-hook
          (lambda () (local-set-key (kbd "C-K") (eshell/clear 1))))

(provide '20-shell)
;;; 20-shell.el ends here
