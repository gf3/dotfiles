;;; init-hltodo.el --- Highlight comment keywords. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package hl-todo
  :straight (:host github :repo "tarsius/hl-todo")
  :config
  (global-hl-todo-mode))

(provide 'init-hltodo)
;;; init-hltodo.el ends here
