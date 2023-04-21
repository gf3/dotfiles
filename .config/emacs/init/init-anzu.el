;;; init-anzu.el --- Search & replace counters. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package anzu
  :straight (:host github :repo "emacsorphanage/anzu")
  :config
  (custom-set-variables
   '(anzu-mode-lighter "")
   '(anzu-deactivate-region t)
   '(anzu-search-threshold 1000)
   '(anzu-replace-threshold 50)
   '(anzu-replace-to-string-separator " → "))

  (define-key isearch-mode-map [remap isearch-query-replace]  #'anzu-isearch-query-replace)
  (define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp)
  :init
  (global-anzu-mode +1))

(provide 'init-anzu)
;;; init-anzu.el ends here
