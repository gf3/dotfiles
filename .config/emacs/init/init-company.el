;; init-company.el --- Autocomplete. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package company
  :straight (:host github :repo "company-mode/company-mode")
  :demand t
  :config
  (define-key company-active-map (kbd "M-/") #'company-complete)
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

(use-package company-quickhelp
  :straight (:host github :repo "company-mode/company-quickhelp")
  :after company
  :init
  (company-quickhelp-mode))

;; (use-package company-box
;;   :straight (:host github :repo "sebastiencs/company-box")
;;   :hook (company-mode . company-box-mode))

(provide 'init-company)
;;; init-company.el ends here
