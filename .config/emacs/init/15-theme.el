;;; init-theme.el --- Theme configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defadvice load-theme (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

(defvar light-theme 'kaolin-valley-light
  "The light theme to use.")
(defvar dark-theme 'kaolin-valley-dark
  "The dark theme to use.")

(use-package kaolin-themes
  :straight (:host github :repo "ogdenwebb/emacs-kaolin-themes")
  :ensure t
  :config
  (setq kaolin-themes-bold t       ; If nil, disable the bold style.
        kaolin-themes-italic t     ; If nil, disable the italic style.
        kaolin-themes-underline t)) ; If nil, disable the underline style.

(use-package auto-dark
  :after kaolin-themes
  :ensure t
  :straight (:host github :repo "LionyxML/auto-dark-emacs")
  :init (auto-dark-mode)
  :custom
  (custome-safe-themes t)
  (auto-dark-themes `((,dark-theme) (,light-theme)))
  :config
  (setq custom-safe-themes t))

(provide 'init-theme)
;;; init-theme.el ends here
