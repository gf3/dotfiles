;;; 20-magit.el --- Magit integration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package magit
  :straight t
  :demand t
  :config
  (setq magit-diff-options '("-b")) ; ignore whitespace
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq magit-define-global-key-bindings t)
  ;; (transient-suffix-put 'magit-dispatch "k" :key "x")
  (setq magit-diff-refine-hunk 'all)
  (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))

  (defalias 'gf3/git
	  (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "g") '("Status" . magit-status))
	    map)
	  "Git")

  (global-set-key (kbd "C-c g") '("Git". gf3/git)))

(use-package forge
  :straight t
  :after magit
  :demand t
  :config
  (transient-append-suffix 'forge-dispatch '(0)
	  ["Edit"
	   ("e a" "assignees" forge-edit-topic-assignees)
	   ("e r" "review requests" forge-edit-topic-review-requests)]))

(use-package magit-delta
  :straight t
  :after magit
  :hook (magit-mode . magit-delta-mode))


(use-package git-link
  :straight t
  :bind
  ("C-c g l" . git-link-dispatch)
  :custom
  (git-link-use-single-line-number t)
  :config
  (require 'git-link-transient))

(provide '20-magit)
;;; 20-magit.el ends here
