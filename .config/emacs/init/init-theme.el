;;; init-theme.el --- Theme configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defadvice load-theme (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

(use-package os1-theme
  :straight (:type git :host github :repo "sashimacs/os1-theme")
  :custom (os1-modeline-padding 12))

(use-package doom-themes
  :straight (:host github :repo "doomemacs/themes"
				   :branch "master")
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t  ; if nil, italics is universally disabled
		doom-themes-padded-modeline t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(defun gf3/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'os1 t))
    ('dark (load-theme 'doom-snazzy t)))
  (set-face-attribute 'mode-line nil
                      :box `(:line-width 12 :color ,(face-attribute 'mode-line :background)))
  (set-face-attribute 'mode-line-inactive nil
                      :box `(:line-width 12 :color ,(face-attribute 'mode-line-inactive :background))))

(add-hook 'after-init-hook (lambda () (gf3/apply-theme 'light)))

(add-hook 'ns-system-appearance-change-functions #'gf3/apply-theme)


(provide 'init-theme)
;;; init-theme.el ends here
