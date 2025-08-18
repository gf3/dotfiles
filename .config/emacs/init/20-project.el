;;; 20-project.el --- Projects. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package project
  :demand t
  :config
  (setq project-vc-extra-root-markers '(".project.el" ".projectile")))

(use-package ibuffer-project
  :straight t
  :demand t
  :after project
  :hook (ibuffer-hook . (lambda ()
                          (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
                          (unless (eq ibuffer-sorting-mode 'project-file-relative)
                            (ibuffer-do-sort-by-project-file-relative)))))

(provide '20-project)
;;; 20-project.el ends here
