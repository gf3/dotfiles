;;; init-perspective.el --- Perspectives for emacs. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package perspective
  :straight (:host github :repo "nex3/perspective-el")
  :after consult
  :demand t
  :custom
  (persp-mode-prefix-key (kbd "C-x x"))
  (persp-suppress-no-prefix-key-warning t)
  :config
  (consult-customize consult--source-buffer :hidden t :default nil)
  (defvar consult--source-perspective
    (list :name     "Perspective"
          :narrow   ?s
          :category 'buffer
          :state    #'consult--buffer-state
          :default  t
          :items    #'persp-get-buffer-names))
  (push consult--source-perspective consult-buffer-sources)
  :init
  (persp-mode))

(provide 'init-perspective)
;;; init-perspective.el ends here
