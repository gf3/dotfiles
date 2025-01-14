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
  (if (project-current)
      (consult-project-buffer)
	(consult-buffer)))

(defun gf3/find ()
  "Find files, maybe scoped to a project."
  (interactive)
  (if (project-current)
	  (consult-project-extra-find)
	(consult-find)))

(defalias 'gf3/buffers
  (let ((map (make-sparse-keymap)))
	(define-key map (kbd "a") '("Last buffer" . switch-to-last-buffer))
	(define-key map (kbd "b") '("Buffers". gf3/buffer))
	(define-key map (kbd "c") '("Symbols" . consult-imenu))
	(define-key map (kbd "f") '("Files" . gf3/find))
	(define-key map (kbd "F") '("Browse files" . find-file))
	(define-key map (kbd "g") '("Grep" . deadgrep))
	(define-key map (kbd "k") '("Kill buffer" . kill-current-buffer))
	(define-key map (kbd "l") '("Lines" . consult-line))
	(define-key map (kbd "n") '("New buffer" . new-buffer))
	(define-key map (kbd "r") '("Recent file" . consult-recent-file))
	map)
  "Buffer management.")

(global-set-key (kbd "C-c b") '("Buffer management". gf3/buffers))

(provide 'init-buffers)
;;; init-buffers.el ends here
