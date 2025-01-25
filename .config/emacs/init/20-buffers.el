;;; init-buffers.el --- Buffer management configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'ibuffer)
(require 'ibuf-ext)

(recentf-mode)

(global-set-key (kbd "C-x C-b") 'ibuffer) ; instead of buffer-list

(setq ibuffer-expert t) ; stop yes no prompt on delete

(use-package all-the-icons-ibuffer
  :straight t
  :demand t)

(use-package ibuffer-vc
  :straight t
  :after (ibuffer all-the-icons-ibuffer ibuffer-project)
  :hook (ibuffer-mode . (lambda ()
                          (ibuffer-vc-set-filter-groups-by-vc-root)
                          (unless (eq ibuffer-sorting-mode 'alphabetic)
                            (ibuffer-do-sort-by-alphabetic))))
  :config
  (require 'all-the-icons-ibuffer)
  (require 'ibuffer-project)
  (setq ibuffer-formats
        '((modified read-only vc-status-mini " "
                    (icon 2 2)
                    (name 30 30 :left :elide)
                    " "
                    (size-h 9 -1 :right)
                    " "
                    (mode+ 16 16 :left :elide)
                    " "
                    (vc-status 16 16 :left)
                    " "
                    project-file-relative))))


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
