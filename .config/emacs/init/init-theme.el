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
  (modus-themes-bold-constructs t)
  (modus-themes-disable-other-themes t)
  (modus-themes-italic-constructs t)
  (modus-themes-mixed-fonts t)
  (modus-themes-prompts '(extrabold italic))
  (modus-themes-to-toggle `(,light-theme ,dark-theme))
  (modus-themes-variable-pitch-ui t))

(use-package circadian
  :straight (:host github :repo "guidoschmidt/circadian.el")
  :ensure t
  :custom
  (calendar-latitude 43.651070)
  (calendar-longitude -79.347015)
  (circadian-themes `((:sunrise . ,light-theme)
                      (:sunset  . ,dark-theme)))
  :config
  (circadian-setup))

(defun gf3/apply-theme (&optional appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (unless appearance (setq appearance 'light))
  (pcase appearance
    ('light (load-theme light-theme t))
    ('dark (load-theme dark-theme t))))

(add-hook 'after-init-hook (lambda () (gf3/apply-theme 'light)))

(add-hook 'ns-system-appearance-change-functions #'gf3/apply-theme)

(provide 'init-theme)
;;; init-theme.el ends here
