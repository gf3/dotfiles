;;; init-copilot.el --- AI code helper buddy guy. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun gf3/copilot-tab ()
  (interactive)
  (or (copilot-accept-completion)
      (indent-for-tab-command)))

(with-eval-after-load 'copilot
  (define-key copilot-mode-map (kbd "<tab>") #'gf3/copilot-tab))

(with-eval-after-load 'company
  ;; disable inline previews
  (delq 'company-preview-if-just-one-frontend company-frontends))

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :custom ((copilot-node-executable "/usr/bin/node" "Set node executable.")
           (copilot-indent-warning-suppress t))
  :hook (prog-mode . copilot-mode)
  :config  
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

  ;; custom completion
  (with-eval-after-load 'copilot
	(define-key copilot-mode-map (kbd "<tab>") #'gf3/copilot-tab)))

(provide 'init-copilot)
;;; init-copilot.el ends here
