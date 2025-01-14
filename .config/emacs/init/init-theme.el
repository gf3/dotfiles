;;; init-theme.el --- Theme configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defadvice load-theme (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

(defvar light-theme 'modus-operandi-tinted
  "The light theme to use.")
(defvar dark-theme 'modus-vivendi-tinted
  "The dark theme to use.")

(use-package modus-themes
  :straight (:host github :repo "protesilaos/modus-themes")
  :ensure t
  :custom
  (modus-themes-mode-line '(accented borderless (padding . 4) (height . 0.9)))
  (modus-themes-hl-line '(accented))
  (modus-themes-paren-match '(bold intense))
  (modus-themes-links '(neutral-underline background))
  (modus-themes-bold-constructs t)
  (modus-themes-disable-other-themes t)
  (modus-themes-italic-constructs t)
  (modus-themes-mixed-fonts t)
  (modus-themes-prompts '(extrabold italic))
  (modus-themes-to-toggle `(,light-theme ,dark-theme))
  (modus-themes-variable-pitch-ui t))

(use-package auto-dark
  :after modus-themes
  :straight (:host github :repo "LionyxML/auto-dark-emacs")
  :init (auto-dark-mode)
  :custom
  (auto-dark-themes `((,dark-theme) (,light-theme)))
  :config
  (setq custom-safe-themes t))

(provide 'init-theme)
;;; init-theme.el ends here
