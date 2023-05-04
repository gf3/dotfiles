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

(defun gf3/buffer ()
  "Find buffers, maybe scoped to a project."
  (interactive)
  (if (projectile-project-p)
	  (consult-project-buffer)
	(consult-buffer)))

(defun gf3/find ()
  "Find files, maybe scoped to a project."
  (interactive)
  (if (projectile-project-p)
	  (consult-projectile-find-file)
	(consult-find)))

(defalias 'gf3/buffers
		  (let ((map (make-sparse-keymap)))
			(define-key map (kbd "a") #'switch-to-last-buffer)
			(define-key map (kbd "b") #'gf3/buffer)
			(define-key map (kbd "c") #'consult-imenu)
			(define-key map (kbd "f") #'gf3/find)
			(define-key map (kbd "g") #'consult-ripgrep)
			(define-key map (kbd "k") #'kill-current-buffer)
			(define-key map (kbd "l") #'consult-line)
			(define-key map (kbd "n") #'new-buffer)
			(define-key map (kbd "r") #'consult-recent-file)
			;; (define-key map (kbd "s") #'consult-eglot-symbols)
			map)
		  "Buffer management.")

(global-set-key (kbd "C-c b") 'gf3/buffers)
(global-set-key (kbd "s-n") 'new-buffer)

(provide 'init-buffers)
;;; init-buffers.el ends here
