;; init-miniframe.el --- Show minibuffer in a child frame. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package mini-frame
  :straight (:host github :repo "muffinmad/emacs-mini-frame")
  :custom
  ((mini-frame-color-shift-step 10)
   (mini-frame-show-parameters '((top . 64)
								 (width . 0.7)
								 (left . 0.5)
								 (child-frame-border-width . 2))))
  :custom-face
  (child-frame-border ((t (:background "systemYellowColor"))))
  :config
  (mini-frame-mode))

(provide 'init-miniframe)
;;; init-miniframe.el ends here
