;;; init-magit.el --- Magit integration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package magit
  :straight (:host github :repo "magit/magit")
  :demand t
  :config
  (setq magit-diff-options '("-b")) ; ignore whitespace
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq magit-define-global-key-bindings t)
  (transient-suffix-put 'magit-dispatch "k" :key "x"))

(use-package forge
  :straight t
  :after magit
  :demand t
  :config
  (transient-append-suffix 'forge-dispatch '(0)
	["Edit"
	 ("e a" "assignees" forge-edit-topic-assignees)
	 ("e r" "review requests" forge-edit-topic-review-requests)]))

(provide 'init-magit)
;;; init-magit.el ends here
