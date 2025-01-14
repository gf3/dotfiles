;;; init-project.el --- Projects. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package project
  :ensure t)

(use-package ibuffer-project
  :straight (ibuffer-project :type git :host github :repo "muffinmad/emacs-ibuffer-project")
  :ensure t
  :after project
  :hook (ibuffer-hook . (lambda ()
                          (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
                          (unless (eq ibuffer-sorting-mode 'project-file-relative)
                            (ibuffer-do-sort-by-project-file-relative)))))

(provide 'init-project)
;;; init-project.el ends here
