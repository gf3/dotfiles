;;; init-project.el --- Projects. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defgroup project-local nil
  "Local, non-VC-backed project.el root directories."
  :group 'project)

(defcustom project-local-identifier ".project.el"
  "You can specify a single filename or a list of names."
  :type '(choice (string :tag "Single file")
                 (repeat (string :tag "Filename")))
  :group 'project-local)

(cl-defmethod project-root ((project (head local)))
  "Return root directory of current PROJECT."
  (cdr project))

(defun project-local-try-local (dir)
  "Determine if DIR is a non-VC project.
DIR must include a file with the name determined by the
variable `project-local-identifier' to be considered a project."
  (if-let ((root (if (listp project-local-identifier)
                     (seq-some (lambda (n)
                                 (locate-dominating-file dir n))
                               project-local-identifier)
                   (locate-dominating-file dir project-local-identifier))))
      (cons 'local root)))

(customize-set-variable 'project-find-functions
                        (list #'project-try-vc
                              #'project-local-try-local))

(use-package project
  :demand t)

(use-package ibuffer-project
  :straight (ibuffer-project :type git :host github :repo "muffinmad/emacs-ibuffer-project")
  :demand t
  :after project
  :hook (ibuffer-hook . (lambda ()
                          (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
                          (unless (eq ibuffer-sorting-mode 'project-file-relative)
                            (ibuffer-do-sort-by-project-file-relative)))))

(provide 'init-project)
;;; init-project.el ends here
