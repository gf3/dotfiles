;; 20-company.el --- Autocomplete. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package company
  :straight t
  :demand t
  :config
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-flip-when-above t)
  :init
  ;; (global-set-key (kbd "<tab>") #'company-indent-or-complete-common)
  (with-eval-after-load 'company
	  (define-key company-active-map (kbd "M-/") #'company-complete)
	  (define-key company-active-map (kbd "TAB") #'company-complete-common-or-cycle)
	  (define-key company-active-map (kbd "<backtab>")
				        (lambda ()
                  (interactive)
                  (company-complete-common-or-cycle -1))))
  (global-company-mode))

(use-package company-box
  :straight t
  :hook (company-mode . company-box-mode))

(use-package company-quickhelp
  :straight t
  :after company
  :init
  (company-quickhelp-mode))

(provide '20-company)
;;; 20-company.el ends here
