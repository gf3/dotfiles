;;; init-buffers.el --- Buffer management configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(recentf-mode)

(defun switch-to-last-buffer ()
  "Switch to the previously open buffer."
  (interactive)
  (switch-to-buffer nil))

(defun new-buffer ()
  "Create a new empty buffer."
  (interactive)
  (let ((buffer (generate-new-buffer "Untitled")))
	(set-buffer-major-mode buffer)
	(switch-to-buffer buffer)))

(defalias 'gf3/buffers
  (let ((map (make-sparse-keymap)))
	(define-key map (kbd "a") #'switch-to-last-buffer)
	(define-key map (kbd "b") #'consult-buffer)
	(define-key map (kbd "f") #'consult-find)
	(define-key map (kbd "g") #'consult-ripgrep)
	(define-key map (kbd "k") #'kill-current-buffer)
	(define-key map (kbd "l") #'consult-line)
	(define-key map (kbd "n") #'new-buffer)
	(define-key map (kbd "r") #'consult-recent-file)
	(define-key map (kbd "s") #'consult-lsp-file-symbols)
	map)
  "Buffer management.")

(global-set-key (kbd "C-c b") 'gf3/buffers)
(global-set-key (kbd "s-n") 'new-buffer)

(provide 'init-buffers)
;;; init-buffers.el ends here
