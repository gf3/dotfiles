;;; init-codereview.el --- Code review. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package code-review
  :after magit
  :straight (:host github :repo "wandersoncferreira/code-review")
  :hook ((code-review-mode . emojify-mode)))

(provide 'init-codereview)
;;; init-codereview.el ends here
