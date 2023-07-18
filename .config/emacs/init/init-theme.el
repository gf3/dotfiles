;;; init-theme.el --- Theme configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defadvice load-theme (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

(use-package ef-themes
  :straight (:type git :host github :repo "protesilaos/ef-themes")
  :demand t
  :custom
  (ef-themes-to-toggle '(ef-day ef-cherie))
  (ef-themes-mixed-fonts t)
  (ef-themes-variable-pitch-ui t)
  (ef-themes-region '(intense no-extend))
  :config
  (ef-themes-select 'ef-day))

;; (use-package os1-theme
;;   :straight (:type git :host github :repo "sashimacs/os1-theme")
;;   :custom (os1-modeline-padding 12))

(defun gf3/apply-theme (&optional appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (unless appearance (setq appearance 'light))
  (pcase appearance
    ('light (load-theme 'ef-day t))
    ('dark (load-theme 'ef-night t)))
  (set-face-attribute 'mode-line nil
                      :box `(:line-width 12 :color ,(face-attribute 'mode-line :background)))
  (set-face-attribute 'mode-line-inactive nil
                      :box `(:line-width 12 :color ,(face-attribute 'mode-line-inactive :background))))

(add-hook 'after-init-hook (lambda () (gf3/apply-theme 'light)))

(add-hook 'ns-system-appearance-change-functions #'gf3/apply-theme)


(provide 'init-theme)
;;; init-theme.el ends here
